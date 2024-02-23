//! A Rustc plugin that prints out the name of all items in a crate.

#![feature(rustc_private)]

pub mod boilerplate;
pub mod nondet;
pub mod state_extraction;
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

fn init_value_for_type(ctx: &Context, ty: String) -> String {
    let map_parts = ty.split("->").collect_vec();
    if map_parts.len() > 1 {
        return "Map()".to_string();
    }

    if ctx.structs.contains_key(&ty) {
        let fields = ctx.structs.get(&ty).unwrap();
        let struct_value = fields
            .iter()
            .map(|field| {
                format!(
                    "{}: {}",
                    field.name,
                    init_value_for_type(ctx, field.ty.clone())
                )
            })
            .collect_vec()
            .join(",");
        return format!("{{ {} }}", struct_value);
    }

    let init_values_by_type: HashMap<&str, &str> = HashMap::from([
        ("List", "List()"),
        ("str", "\"\""),
        ("int", "0"),
        ("Addr", "\"s1\""),
    ]);

    init_values_by_type
        .get(ty.as_str())
        .unwrap_or_else(|| {
            eprintln!("No init value for type: {ty}");
            &"<missing-type>"
        })
        .to_string()
}

fn translation_priority(item: &rustc_hir::Item<'_>) -> i32 {
    match item.kind {
        rustc_hir::ItemKind::Struct(..) => 1,
        rustc_hir::ItemKind::Enum(..) => 2,
        rustc_hir::ItemKind::Fn(..) => 3,
        _ => 4,
    }
}

fn visit_item(ctx: &mut Context, item: rustc_hir::Item) {
    let translation = item.translate(ctx);
    if !translation.is_empty() {
        println!("{}\n", translation);
    }
}

fn translate_items(tcx: TyCtxt) -> TyCtxt {
    let mut ctx = Context {
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
        structs: HashMap::new(),
        stateful_ops: vec![],
        record_fields: vec![],
        current_item_name: "".to_string(),
        tcx,
        contract_state: vec![],
    };

    let items = tcx
        .hir()
        .items()
        .map(|item_id| tcx.hir().item(item_id))
        .sorted_by(|a, b| translation_priority(a).cmp(&translation_priority(b)));
    items.for_each(|item| visit_item(&mut ctx, *item));

    let contract_state = ctx
        .contract_state
        .iter()
        .map(|x| format!("{}: {}", x.0, x.1))
        .collect_vec()
        .join(",\n  ");
    println!("  type ContractState = {{\n    {contract_state}\n  }}\n");

    let initializer = "  pure val init_contract_state = {\n".to_string()
        + &ctx
            .contract_state
            .iter()
            .map(|field| {
                format!(
                    "    {}: {}",
                    field.0,
                    init_value_for_type(&ctx, field.1.clone())
                )
            })
            .collect_vec()
            .join(",\n")
        + "\n  }";
    println!("{initializer}");

    let actions = ctx
        .stateful_ops
        .iter()
        .filter(|op| *op != &"execute".to_string() && *op != &"instantiate".to_string())
        .map(|op| format!("{op}_action"))
        .collect_vec()
        .join(",\n      ");
    println!(
        "
  action execute_step = all {{
    any {{
      {actions}
    }},
    advance_time,
    bank' = bank,
  }}
"
    );
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
    translate_items(tcx);
    print!("{INITIALIZERS}");
    print!("{ACTIONS}");
    println!("}}");
}
