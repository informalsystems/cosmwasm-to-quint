//! A Rustc plugin that prints out the name of all items in a crate.

#![feature(rustc_private)]

pub mod translate;

extern crate itertools;
extern crate rustc_ast;
extern crate rustc_driver;
extern crate rustc_hir;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_session;
extern crate rustc_span;
extern crate rustc_type_ir;

use std::{borrow::Cow, env, process::Command};

use clap::Parser;
use itertools::Itertools;
use rustc_hir::intravisit::{self, Visitor};
use rustc_middle::ty::TyCtxt;
use rustc_plugin::{CrateFilter, RustcPlugin, RustcPluginArgs, Utf8Path};
use serde::{Deserialize, Serialize};

use crate::translate::{
    translate_expr, translate_fn_decl, translate_variant_data, try_to_translate_state_var_info,
};

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

fn visit_test(tcx: TyCtxt) -> TyCtxt {
    struct Finder<'tcx> {
        tcx: TyCtxt<'tcx>,
        contract_state: Vec<String>,
    }

    impl<'tcx> Visitor<'tcx> for Finder<'tcx> {
        fn visit_item(&mut self, item: &'tcx rustc_hir::Item<'tcx>) {
            let name = item.ident;
            let should_skip = name.as_str().starts_with('_')
                || ["", "FIELDS", "VARIANTS"].contains(&name.as_str())
                || name.as_str().starts_with("query") // skip query functions for now
                || name.as_str().starts_with("Query");
            if should_skip {
                return;
            }

            match item.kind {
                rustc_hir::ItemKind::Const(_ty, _generics, body) => {
                    let const_item = self.tcx.hir().body(body);
                    // println!("{const_item:#?}");
                    let ret = try_to_translate_state_var_info(self.tcx, *const_item);
                    match ret {
                        Some(ret) => {
                            self.contract_state.push(format!("{name}: {ret}"));
                        }
                        None => {
                            let ret2 = translate_expr(*const_item.value, &vec![]);
                            println!("pure val {name} = {ret2}");
                            let ret3 = const_item.params;
                            if !ret3.is_empty() {
                                println!("{ret3:#?}")
                            }
                        }
                    }
                }

                rustc_hir::ItemKind::Struct(variant_data, _generics) => {
                    // println!("Found a struct: {variant_data:#?}");
                    let fields = translate_variant_data(variant_data);
                    println!("type {name} = {fields}")
                }

                rustc_hir::ItemKind::Enum(enum_def, _generics) => {
                    //  println!("Found an enum: {enum_def:#?}");
                    let variants = enum_def
                        .variants
                        .iter()
                        .map(|variant| {
                            let ident = variant.ident;
                            if variant.data.fields().is_empty() {
                                return format!("  | {name}_{ident}");
                            }
                            let fields = translate_variant_data(variant.data);
                            let ret = format!("  | {name}_{ident}({fields})");
                            ret
                        })
                        .collect_vec()
                        .join("\n");
                    println!("type {name} =\n{variants}")
                }

                rustc_hir::ItemKind::Fn(sig, _generics, body_id) => {
                    let body = self.tcx.hir().body(body_id);
                    let (sig, has_state) = translate_fn_decl(*sig.decl, *body);
                    let body_value = translate_expr(*body.value, &vec![]);
                    if has_state {
                        println!("pure def {name}{sig} = ({body_value}, contract_state)");
                    } else {
                        println!("pure def {name}{sig} = {body_value}");
                    }
                }
                _ => {
                    // let m = format!("other ({})", item.kind.descr());
                    // println!("{m}")
                }
            };

            intravisit::walk_item(self, item)
        }
    }

    let mut finder = Finder {
        tcx,
        contract_state: vec![],
    };
    tcx.hir().visit_all_item_likes_in_crate(&mut finder);

    let contract_state = finder.contract_state.join(",\n  ");
    println!("type ContractState = {{\n  {contract_state}\n}}");

    tcx
}

// The core of our analysis. It doesn't do much, just access some methods on the `TyCtxt`.
// I recommend reading the Rustc Development Guide to better understand which compiler APIs
// are relevant to whatever task you have.
fn cosmwasm_to_quint(tcx: TyCtxt, _args: &CosmwasmToQuintPluginArgs) {
    visit_test(tcx);
}
