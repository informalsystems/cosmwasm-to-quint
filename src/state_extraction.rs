use itertools::Itertools;
use rustc_span::{symbol::Ident, Symbol};

use crate::{translate::Translatable, types::Context};

impl Translatable for rustc_middle::ty::Ty<'_> {
    fn translate(&self, ctx: &mut Context) -> String {
        // FIXME: This should be quite unstable, but I couldn't figure out how to navigate `ty` here
        let var_name = &format!("{:#?}", self);
        let name = var_name
            .split("::")
            .last()
            .unwrap()
            .split(' ')
            .last()
            .unwrap()
            .split(')')
            .next()
            .unwrap();

        Ident::with_dummy_span(Symbol::intern(name)).translate(ctx)
    }
}
impl Translatable for rustc_middle::ty::GenericArg<'_> {
    fn translate(&self, ctx: &mut Context) -> String {
        let kind = self.unpack();
        match kind {
            rustc_middle::ty::GenericArgKind::Type(ty) => ty.translate(ctx),
            rustc_middle::ty::GenericArgKind::Const(_) => "".to_string(),
            rustc_middle::ty::GenericArgKind::Lifetime(_) => "".to_string(),
        }
    }
}

pub fn try_to_translate_state_var_info(ctx: &mut Context, body: rustc_hir::Body) -> Option<String> {
    if let rustc_hir::ExprKind::Call(expr, _) = body.value.kind {
        if let rustc_hir::ExprKind::Path(rustc_hir::QPath::TypeRelative(ty, _segment)) = expr.kind {
            // let method = segment.ident;
            // let method_args = segment.args;
            // println!("Method: {method} ({method_args:#?})");
            if let rustc_hir::TyKind::Path(rustc_hir::QPath::Resolved(_, path)) = ty.kind {
                if let rustc_hir::def::Res::Def(_kind, def_id) = path.res {
                    let crate_name = ctx.tcx.crate_name(def_id.krate);
                    if crate_name.as_str() != "cw_storage_plus" {
                        return None;
                    }

                    // if crate name is `cw_storage_plus`, we need to translate
                    // this into part of the contract state

                    let def_id = expr.hir_id.owner.def_id; 
                    let ty = ctx.tcx.typeck(def_id).node_type(expr.hir_id);
                    if let rustc_type_ir::TyKind::FnDef(_ty_def_id, generic_args) = ty.kind() {
                        // We need to look into the GenericArgs here (from FnDef)
                        // to annotate the quint state variable properly

                        let translated_types =
                            generic_args.iter().map(|t| t.translate(ctx)).collect_vec();

                        // For Map::new, the 2nd and 3rd generics are the map types
                        // For Item::new, the 2nd generic is the type
                        // Are there other options?
                        let translated_type = if path.segments[0].ident.as_str().starts_with("Map")
                        {
                            let ret = format!("{} -> {}", translated_types[1], translated_types[2]);
                            ret.to_string()
                        } else {
                            translated_types[1].clone()
                        };

                        return Some(translated_type);
                    }
                };
            }
        }
    }
    None
}
