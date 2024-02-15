//! A Rustc plugin that prints out the name of all items in a crate.

#![feature(rustc_private)]

extern crate rustc_driver;
extern crate rustc_hir;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_session;
extern crate rustc_span;
extern crate itertools;

use std::{borrow::Cow, env, process::Command};

use clap::Parser;
use itertools::Itertools;
use rustc_hir::{intravisit::{self, Visitor, Map}, BodyId, FnDecl, TyKind, VariantData};
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
      .enter(|tcx| print_all_items(tcx, &self.args));

    // Note that you should generally allow compilation to continue. If
    // your plugin is being invoked on a dependency, then you need to ensure
    // the dependency is type-checked (its .rmeta file is emitted into target/)
    // so that its dependents can read the compiler outputs.
    rustc_driver::Compilation::Continue
  }
}

fn translate_variant_data(variant_data: VariantData) -> String {
  // println!("Found a struct: {variant_data:#?}");
  let mut fields = variant_data.fields().into_iter().map(|field| {
    let field_ident = field.ident.to_string();
    let field_type = if let TyKind::Path(rustc_hir::QPath::Resolved(_, path)) = field.ty.kind {
      let strings = path.segments.into_iter().map(|seg| seg.ident.as_str().to_string() );
      let a: Vec<String> = strings.collect_vec();
      a.join("::")
    } else {
      "".to_string()
    };

    let ret =format!("{} : {}", field_ident, field_type);
    ret
  });

  let ret = format!("{{ {} }}", fields.join(", "));
  ret
}

fn visit_test(tcx: TyCtxt) -> TyCtxt {
  struct Finder<'tcx> {
    tcx: TyCtxt<'tcx>,
  }

  impl<'tcx> Visitor<'tcx> for Finder<'tcx> {
    fn visit_item(&mut self, item: &'tcx rustc_hir::Item<'tcx>) {
      let msg = format!(
        "There is an item \"{}\" of type \"{}\"",
        item.ident,
        item.kind.descr()
      );
      println!("{msg}");

      match item.kind {
        rustc_hir::ItemKind::Const(ty, generics, body) => {
          let const_item = self.tcx.hir().body(body);
          // println!("{const_item:#?}");
          if let rustc_hir::ExprKind::Call(expr, _) = const_item.value.kind {
            if let rustc_hir::ExprKind::Path(rustc_hir::QPath::TypeRelative(ty, segment)) = expr.kind {
              let method = segment.ident;
              let method_args = segment.args;
              if let TyKind::Path(rustc_hir::QPath::Resolved(_, path)) = ty.kind {
                for seg in path.segments {
                  let class_name = seg.ident;

                  let msg = format!(
                    "Constant {} = {} :: {} ({:#?})",
                    item.ident,
                    class_name,
                    method,
                    method_args
                  );
                  println!("{msg}")
                }
                if let rustc_hir::def::Res::Def(_, id) = path.res {
                  let crate_name = self.tcx.crate_name(id.krate);
                  println!("Crate name: {crate_name:#?}");
                  // if crate name is `cw_storage_plus`, we need to translate
                  // this into part of the contract state
                }
              }
            }
          }
        }

        rustc_hir::ItemKind::Struct(variant_data, generics) => {
          // println!("Found a struct: {variant_data:#?}");
          let fields = translate_variant_data(variant_data);
          let msg = format!(
            "Struct {} = {}",
            item.ident,
            fields,
          );
          println!("{msg}")
        }

        rustc_hir::ItemKind::TyAlias(ty, generics) => {
           println!("Found a type alias: {ty:#?}");
        }


        rustc_hir::ItemKind::Enum(enum_def, generics ) => {
           println!("Found an enum: {enum_def:#?}");
           for variant in enum_def.variants {
             let ident = variant.ident;
             let fields = translate_variant_data(variant.data);
             let msg = format!(
               "Enum {} = | {}({})",
               item.ident,
               ident,
               fields,
             );
             // TODO multiple variants should be in the same enum
             println!("{msg}")
           }
        }

        rustc_hir::ItemKind::Fn(sig, generics, body) => { println!("Fn: {body:#?}")}
        _ => { let m = format!("other ({})", item.kind.descr()); println!("{m}") }
      };

      let decl = self.tcx.hir().fn_decl_by_hir_id(item.hir_id());
      println!("{decl:?}");

      intravisit::walk_item(self, item)
    }

    // fn visit_fn(&mut self, fk: intravisit::FnKind<'tcx>, fd: &'tcx rustc_hir::FnDecl<'tcx>, b: BodyId, _: rustc_span::Span, id: rustc_span::def_id::LocalDefId) {
    //   let ident = match fk {
    //     intravisit::FnKind::ItemFn(ident, _, _) => { ident.as_str().to_string() }
    //     intravisit::FnKind::Method(ident, _) => { ident.as_str().to_string() }
    //     intravisit::FnKind::Closure => { "".to_string() }
    //   };
    //   println!("{ident}")
    // }

    // fn visit_fn_decl(&mut self, fd: &'tcx rustc_hir::FnDecl<'tcx>) {
    //   let msg = format!(
    //     "FUN DECL returning \"{:?}\"",
    //     fd.output,
    //   );
    //   println!("{msg}");
    // }

    fn visit_body(&mut self, body: &'tcx rustc_hir::Body<'tcx>) {
      // intravisit::walk_body(self, body);
      let expr: &rustc_hir::Expr<'_> = body.value;
      let k: rustc_hir::ExprKind<'_> = expr.kind;
      let id= expr.hir_id;
      println!("{id}");

      // let local_def_id = self.tcx.hir().body_owner_def_id(id);
      // let function_path = self
      //   .tcx
      //   .def_path(local_def_id.to_def_id())
      //   .to_string_no_crate_verbose();
      // if function_path[1 ..] == self.qpath {
      //   self.span = Some(self.tcx.hir().span(id.hir_id));
      // }
    }
  }

  let mut finder = Finder {
    tcx,
  };
  tcx.hir().visit_all_item_likes_in_crate(&mut finder);
  tcx

}

// The core of our analysis. It doesn't do much, just access some methods on the `TyCtxt`.
// I recommend reading the Rustc Development Guide to better understand which compiler APIs
// are relevant to whatever task you have.
fn print_all_items(tcx: TyCtxt, args: &CosmwasmToQuintPluginArgs) {
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
