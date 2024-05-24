//! A Rustc plugin that generates quint stubs out of CosmWasm Rust contracts.

#![feature(rustc_private)]

pub mod boilerplate;
pub mod nondet;
pub mod state_extraction;
pub mod test_generation;
pub mod translate;
pub mod types;

extern crate itertools;
extern crate rustc_ast;
extern crate rustc_driver;
extern crate rustc_hir;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_session;
extern crate rustc_span;
extern crate rustc_type_ir;

use std::{borrow::Cow, collections::HashMap, env, process::Command};

use clap::Parser;
use itertools::Itertools;
use rustc_middle::ty::TyCtxt;
use rustc_plugin::{CrateFilter, RustcPlugin, RustcPluginArgs, Utf8Path};
use serde::{Deserialize, Serialize};
use translate::Translatable;

use crate::types::{Constructor, Context};

use crate::boilerplate::{post_items, pre_items};
use crate::test_generation::generate_tests;

// This struct is the plugin provided to the rustc_plugin framework,
// and it must be exported for use by the CLI/driver binaries.
pub struct CosmwasmToQuintPlugin;

// No plugin-specific args for now, but we'll use this to pass through
// arguments to Cargo.
#[derive(Parser, Serialize, Deserialize)]
pub struct CosmwasmToQuintPluginArgs {
    #[clap(last = true)]
    cargo_args: Vec<String>,
}

impl RustcPlugin for CosmwasmToQuintPlugin {
    type Args = CosmwasmToQuintPluginArgs;

    fn version(&self) -> Cow<'static, str> {
        env!("CARGO_PKG_VERSION").into()
    }

    fn driver_name(&self) -> Cow<'static, str> {
        "cosmwasm-to-quint-driver".into()
    }

    fn args(&self, _target_dir: &Utf8Path) -> RustcPluginArgs<Self::Args> {
        let args = CosmwasmToQuintPluginArgs::parse_from(env::args().skip(1));
        let filter = CrateFilter::AllCrates;
        RustcPluginArgs { args, filter }
    }

    // Pass Cargo arguments (like --feature) from the top-level CLI to Cargo.
    fn modify_cargo(&self, cargo: &mut Command, args: &Self::Args) {
        cargo.args(&args.cargo_args);
    }

    // In the driver, we use the Rustc API to start a compiler session
    // for the arguments given to us by rustc_plugin.
    fn run(
        self,
        compiler_args: Vec<String>,
        plugin_args: Self::Args,
    ) -> rustc_interface::interface::Result<()> {
        let mut callbacks = CosmwasmToQuintCallbacks { args: plugin_args };
        let compiler = rustc_driver::RunCompiler::new(&compiler_args, &mut callbacks);
        compiler.run()
    }
}

struct CosmwasmToQuintCallbacks {
    args: CosmwasmToQuintPluginArgs,
}

impl rustc_driver::Callbacks for CosmwasmToQuintCallbacks {
    // At the top-level, the Rustc API uses an event-based interface for
    // accessing the compiler at different stages of compilation. In this callback,
    // all the type-checking has completed.

    fn after_analysis<'tcx>(
        &mut self,
        _compiler: &rustc_interface::interface::Compiler,
        queries: &'tcx rustc_interface::Queries<'tcx>,
    ) -> rustc_driver::Compilation {
        // We extract a key data structure, the `TyCtxt`, which is all we need
        // for accessing the high-level IR and its types.
        queries
            .global_ctxt()
            .unwrap()
            .enter(|tcx| cosmwasm_to_quint(tcx, &self.args));

        // Note that you should generally allow compilation to continue. If
        // your plugin is being invoked on a dependency, then you need to ensure
        // the dependency is type-checked (its .rmeta file is emitted into target/)
        // so that its dependents can read the compiler outputs.
        rustc_driver::Compilation::Continue
    }
}

fn translation_priority(item: &rustc_hir::Item<'_>) -> i32 {
    match item.kind {
        rustc_hir::ItemKind::Struct(..) => 1,
        rustc_hir::ItemKind::Enum(..) => 2,
        rustc_hir::ItemKind::Fn(..) => 3,
        _ => 4,
    }
}

fn translate_all_items(tcx: TyCtxt) {
    let items_by_crate = tcx
        .hir()
        .items()
        .map(|item_id| tcx.hir().item(item_id))
        .group_by(|item| item.hir_id().owner.def_id.to_def_id().krate);

    items_by_crate.into_iter().for_each(|(crate_id, items)| {
        let crate_name = tcx.crate_name(crate_id);
        translate_items(tcx, crate_name.as_str(), items.collect_vec())
    });
}

fn translate_items(tcx: TyCtxt, crate_name: &str, items: Vec<&rustc_hir::Item>) {
    let mut ctx = Context {
        tcx,
        crate_name,
        message_type_for_action: HashMap::from([(
            "instantiate".to_string(),
            "InstantiateMsg".to_string(),
        )]),
        constructors: HashMap::from([(
            "Ok".to_string(),
            Constructor {
                name: "Ok".to_string(),
                result_type: "Result".to_string(),
                fields: vec![],
            },
        )]),
        structs: HashMap::new(),
        ops_with_mutability: vec![],
        contract_state: vec![],
        nondet_picks: vec![
            ("sender".to_string(), "Addr".to_string()),
            ("denom".to_string(), "str".to_string()),
            ("amount".to_string(), "int".to_string()),
        ],
        // scoped
        record_fields: vec![],
        struct_fields: vec![],
        pat_fields: vec![],
        current_item_name: "".to_string(),
    };

    // Iterate over all crate items, sorted by translation priority, translating
    // each one and filtering out empty translations (i.e. from ignored items)
    let translated_items = items
        .iter()
        .sorted_by(|a, b| translation_priority(a).cmp(&translation_priority(b)))
        .map(|item| item.translate(&mut ctx))
        .filter(|translation| !translation.is_empty())
        .collect_vec()
        .join("\n");

    if ctx.contract_state.is_empty() {
        eprintln!("No contract state found for crate: {crate_name}. Skipping.");
        // FIXME: this should be translated into a stateless module
        return;
    }

    let module = format!(
        "{}\n{}\n{}\n",
        pre_items(crate_name),
        translated_items,
        post_items(&ctx)
    );
    let tests = generate_tests(ctx.clone());

    // write module to file
    std::fs::write(format!("quint/{}_stubs.qnt", crate_name), module)
        .expect("Unable to write file");

    // write tests to file
    std::fs::write(format!("tests/mbt_{}.rs", crate_name), tests).expect("Unable to write file");
}

// This is the main entry point for the plugin. It prints the generated quint code to STDOUT.
fn cosmwasm_to_quint(tcx: TyCtxt, _args: &CosmwasmToQuintPluginArgs) {
    // create directories for the output files (if they don't already exist)
    std::fs::create_dir_all("quint/lib").expect("Unable to create directory");
    std::fs::create_dir_all("tests").expect("Unable to create directory");

    // Read quint lib files. `include_str!` makes it so they are read at
    // compilation time, and therefore get embeded in the cosmwasm-to-quint executable
    let bank = include_str!("./quint-lib-files/bank.qnt");
    let basic_spells = include_str!("./quint-lib-files/basicSpells.qnt");
    let bounded_uint = include_str!("./quint-lib-files/BoundedUInt.qnt");
    let cw_types = include_str!("./quint-lib-files/cw_types.qnt");
    let cw_utils = include_str!("./quint-lib-files/cw_utils.qnt");
    let messaging = include_str!("./quint-lib-files/messaging.qnt");

    // Write the lib files in runtime
    std::fs::write("quint/lib/bank.qnt", bank).expect("Unable to write file");
    std::fs::write("quint/lib/basicSpells.qnt", basic_spells).expect("Unable to write file");
    std::fs::write("quint/lib/BoundedUInt.qnt", bounded_uint).expect("Unable to write file");
    std::fs::write("quint/lib/cw_types.qnt", cw_types).expect("Unable to write file");
    std::fs::write("quint/lib/cw_utils.qnt", cw_utils).expect("Unable to write file");
    std::fs::write("quint/lib/messaging.qnt", messaging).expect("Unable to write file");

    translate_all_items(tcx);
}
