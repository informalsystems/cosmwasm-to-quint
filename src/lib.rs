//! A Rustc plugin that prints out the name of all items in a crate.

#![feature(rustc_private)]

extern crate itertools;
extern crate rustc_ast;
extern crate rustc_driver;
extern crate rustc_hir;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_session;
extern crate rustc_span;

use std::{borrow::Cow, env, process::Command};

use clap::Parser;
use itertools::Itertools;
use rustc_ast::LitKind;
use rustc_hir::{
    intravisit::{self, Visitor},
    TyKind, VariantData,
};
use rustc_middle::ty::TyCtxt;
use rustc_plugin::{CrateFilter, RustcPlugin, RustcPluginArgs, Utf8Path};
use serde::{Deserialize, Serialize};

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

fn segment_to_string(segments: &[rustc_hir::PathSegment]) -> String {
    let strings = segments.iter().map(|seg| seg.ident.as_str().to_string());
    let a: Vec<String> = strings.collect_vec();
    a.join("::")

    // TODO: convert arguments
    // let method_args = segment.args;
}

fn translate_variant_data(variant_data: VariantData) -> String {
    // println!("Found a struct: {variant_data:#?}");
    let mut fields = variant_data.fields().iter().map(|field| {
        let field_ident = field.ident.to_string();
        let field_type = if let TyKind::Path(rustc_hir::QPath::Resolved(_, path)) = field.ty.kind {
            segment_to_string(path.segments)
        } else {
            "".to_string()
        };

        let ret = format!("{} : {}", field_ident, field_type);
        ret
    });

    let ret = format!("{{ {} }}", fields.join(", "));
    ret
}

fn translate_lit(lit: &LitKind) -> String {
    match lit {
        LitKind::Str(sym, rustc_ast::StrStyle::Cooked) => sym.to_string(),
        LitKind::Int(i, _) => i.to_string(),
        _ => "".to_string(),
    }
}

fn translate_expr(expr: rustc_hir::Expr) -> String {
    match expr.kind {
        rustc_hir::ExprKind::Lit(lit) => translate_lit(&lit.node),
        rustc_hir::ExprKind::Binary(op, e1, e2) => {
            let s1 = translate_expr(*e1);
            let s2 = translate_expr(*e2);
            let ret = format!("{} {} {}", s1, op.node.as_str(), s2);
            ret
        }
        _ => "".to_string(),
    }
}

fn try_to_translate_state_var_info(tcx: TyCtxt, body: rustc_hir::Body) -> String {
    if let rustc_hir::ExprKind::Call(expr, _) = body.value.kind {
        if let rustc_hir::ExprKind::Path(rustc_hir::QPath::TypeRelative(ty, segment)) = expr.kind {
            let _method = segment.ident;
            let _method_args = segment.args;
            if let TyKind::Path(rustc_hir::QPath::Resolved(_, path)) = ty.kind {
                if let rustc_hir::def::Res::Def(_, def_id) = path.res {
                    let crate_name = tcx.crate_name(def_id.krate);
                    println!("Crate name: {crate_name:#?}");
                    // if crate name is `cw_storage_plus`, we need to translate
                    // this into part of the contract state
                    // ------------

                    let def_id = expr.hir_id.owner.def_id; // def_id identifies the main function
                    let ty = tcx.typeck(def_id).node_type(expr.hir_id);
                    println!("Type: {ty:#?}");
                    // We need to look into the GenericArgs here (from FnDef)
                    // to annotate the quint state variable properly

                    // For Map::new, the 2nd and 3rd generics are the map types
                    // For Item::new, the 2nd generic is the type
                    // Are there other options?
                };

                segment_to_string(path.segments)
            } else {
                "".to_string()
            }
        } else {
            "".to_string()
        }
    } else {
        "".to_string()
    }
}

fn translate_type(ty: rustc_hir::Ty) -> String {
    match ty.kind {
        TyKind::Path(rustc_hir::QPath::Resolved(_, path)) => segment_to_string(path.segments),
        _ => "".to_string(),
    }
}

fn translate_fn_decl(decl: rustc_hir::FnDecl) -> String {
    let inputs = decl.inputs.iter().map(|input| translate_type(*input));

    let output = match decl.output {
        rustc_hir::FnRetTy::DefaultReturn(_) => "void".to_string(),
        rustc_hir::FnRetTy::Return(ty) => translate_type(*ty),
    };
    let ret = format!("({}): {}", inputs.collect_vec().join(", "), output);
    ret
}

fn visit_test(tcx: TyCtxt) -> TyCtxt {
    struct Finder<'tcx> {
        tcx: TyCtxt<'tcx>,
    }

    impl<'tcx> Visitor<'tcx> for Finder<'tcx> {
        fn visit_item(&mut self, item: &'tcx rustc_hir::Item<'tcx>) {
            let name = item.ident;
            // let msg = format!(
            //   "There is an item \"{}\" of type \"{}\"",
            //   item.ident,
            //   item.kind.descr()
            // );
            // println!("{msg}");

            match item.kind {
                rustc_hir::ItemKind::Const(_ty, _generics, body) => {
                    let const_item = self.tcx.hir().body(body);
                    // println!("{const_item:#?}");
                    let ret = try_to_translate_state_var_info(self.tcx, *const_item);
                    println!("(TODO) state var? {name}: {ret}");
                    let ret2 = translate_expr(*const_item.value);
                    println!("Constant {name} = {ret2}")
                }

                rustc_hir::ItemKind::Struct(variant_data, _generics) => {
                    // println!("Found a struct: {variant_data:#?}");
                    let fields = translate_variant_data(variant_data);
                    println!("Struct {name} = {fields}")
                }

                rustc_hir::ItemKind::TyAlias(ty, _generics) => {
                    println!("Found a type alias: {ty:#?}");
                }

                rustc_hir::ItemKind::Enum(enum_def, _generics) => {
                    //  println!("Found an enum: {enum_def:#?}");
                    for variant in enum_def.variants {
                        let ident = variant.ident;
                        let fields = translate_variant_data(variant.data);
                        // TODO multiple variants should be in the same enum
                        println!("Enum {name} = | {ident}({fields})")
                    }
                }

                rustc_hir::ItemKind::Fn(sig, _generics, _bodyy) => {
                    let ret = translate_fn_decl(*sig.decl);
                    println!("Fn: {name} with type {ret}")
                }
                _ => {
                    // let m = format!("other ({})", item.kind.descr());
                    // println!("{m}")
                }
            };

            intravisit::walk_item(self, item)
        }
    }

    let mut finder = Finder { tcx };
    tcx.hir().visit_all_item_likes_in_crate(&mut finder);
    tcx
}

// The core of our analysis. It doesn't do much, just access some methods on the `TyCtxt`.
// I recommend reading the Rustc Development Guide to better understand which compiler APIs
// are relevant to whatever task you have.
fn cosmwasm_to_quint(tcx: TyCtxt, _args: &CosmwasmToQuintPluginArgs) {
    visit_test(tcx);
    // let hir = tcx.hir();
    // for item_id in hir.items() {
    //   let item = hir.item(item_id);
    //   let mut msg = format!(
    //     "There is an item \"{}\" of type \"{}\"",
    //     item.ident,
    //     item.kind.descr()
    //   );
    //   if args.caps {
    //     msg = msg.to_uppercase();
    //   }
    //   println!("{msg}");
    // }
}
