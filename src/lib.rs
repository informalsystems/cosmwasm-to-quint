//! A Rustc plugin that prints out the name of all items in a crate.

#![feature(rustc_private)]

pub mod boilerplate;
pub mod translate;
pub mod visitors;

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

use crate::translate::{Constructor, Context};

use crate::visitors::{OpTranslator, TypeTranslator};

use crate::boilerplate::{ACTIONS, CONTRACT_ADDRESS, IMPORTS, INITIALIZERS, VALUES, VARS};

// This struct is the plugin provided to the rustc_plugin framework,
// and it must be exported for use by the CLI/driver binaries.
pub struct CosmwasmToQuintPlugin;

// To parse CLI arguments, we use Clap for this example. But that
// detail is up to you.
#[derive(Parser, Serialize, Deserialize)]
pub struct CosmwasmToQuintPluginArgs {
    #[arg(short, long)]
    caps: bool,

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

    // In the CLI, we ask Clap to parse arguments and also specify a CrateFilter.
    // If one of the CLI arguments was a specific file to analyze, then you
    // could provide a different filter.
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
        // for our simple task of printing out item names.
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

fn init_value_for_type(ty: String) -> String {
    let map_parts = ty.split("->").collect_vec();
    if map_parts.len() > 1 {
        return format!("Map()");
    }
    let init_values_by_type: HashMap<&str, &str> =
        HashMap::from([("List", "List()"), ("str", "\"\""), ("int", "0")]);
    init_values_by_type.get(ty.as_str()).unwrap().to_string()
}

fn visit_test(tcx: TyCtxt) -> TyCtxt {
    let ctx = Context {
        message_type_for_action: HashMap::from([(
            "instantiate".to_string(),
            "InstantiateMsg".to_string(),
        )]),
        constructors: HashMap::from([(
            "Response_Ok".to_string(),
            Constructor {
                name: "Response_Ok".to_string(),
                fields: vec![],
            },
        )]),
        stateful_ops: vec![],
    };

    let mut type_translator = TypeTranslator {
        tcx,
        contract_state: vec![],
        ctx,
    };
    tcx.hir()
        .visit_all_item_likes_in_crate(&mut type_translator);

    let mut op_translator = OpTranslator {
        tcx,
        ctx: type_translator.ctx,
        contract_state: type_translator.contract_state,
    };
    tcx.hir().visit_all_item_likes_in_crate(&mut op_translator);
    let contract_state = op_translator
        .contract_state
        .iter()
        .map(|x| format!("{}: {}", x.0, x.1))
        .collect_vec()
        .join(",\n  ");
    println!("type ContractState = {{\n  {contract_state}\n}}");

    let initializer = "pure val init_contract_state = {\n".to_string()
        + &op_translator
            .contract_state
            .iter()
            .map(|field| format!("  {}: {}\n", field.0, init_value_for_type(field.1.clone())))
            .collect_vec()
            .join(",\n")
        + "}";
    println!("{initializer}");

    tcx
}

// The core of our analysis. It doesn't do much, just access some methods on the `TyCtxt`.
// I recommend reading the Rustc Development Guide to better understand which compiler APIs
// are relevant to whatever task you have.
fn cosmwasm_to_quint(tcx: TyCtxt, _args: &CosmwasmToQuintPluginArgs) {
    println!("module generated {{");
    print!("{IMPORTS}");
    print!("{VARS}");
    print!("{CONTRACT_ADDRESS}");
    print!("{VALUES}");
    print!("{INITIALIZERS}");
    print!("{ACTIONS}");
    visit_test(tcx);
    println!("}}");
}
