use std::collections::HashMap;

use itertools::Itertools;
use rustc_ast::LitKind;
use rustc_hir::{TyKind, VariantData};
use rustc_middle::ty::TyCtxt;
use std::iter::zip;

pub fn segment_to_string(segments: &[rustc_hir::PathSegment]) -> String {
    let translated_segments: HashMap<&str, &str> = HashMap::from([
        ("Vec", "List"),
        ("String", "str"),
        ("Uint128", "int"),
        ("u128", "int"),
        ("Timestamp", "int"),
    ]);

    let strings = segments.iter().map(|seg| {
        let s = seg.ident.as_str();
        let translated = translated_segments.get(s);
        match translated {
            Some(t) => t.to_string(),
            None => s.to_string(),
        }
    });
    let a: Vec<String> = strings.collect_vec();
    a.join("::")

    // TODO: convert arguments
    // let method_args = segment.args;
}

pub fn translate_param(param: rustc_hir::Param) -> String {
    match param.pat.kind {
        rustc_hir::PatKind::Binding(_a, _id, name, _n) => name.as_str().to_string(),
        _ => {
            let ret = format!("pat<{:#?}>", param.pat.kind);
            ret
        }
    }
}

pub fn translate_variant_data(variant_data: VariantData) -> String {
    // println!("Found a struct: {variant_data:#?}");
    let mut fields = variant_data.fields().iter().map(|field| {
        let field_ident = field.ident.to_string();
        let field_type = translate_type(*field.ty);
        let ret = format!("{}: {}", field_ident, field_type);
        ret
    });

    let ret = format!("{{ {} }}", fields.join(", "));
    ret
}

pub fn translate_lit(lit: &LitKind) -> String {
    match lit {
        LitKind::Str(sym, rustc_ast::StrStyle::Cooked) => {
            let ret = format!("\"{}\"", sym.as_str());
            ret
        }
        LitKind::Int(i, _) => i.to_string(),
        _ => "".to_string(),
    }
}

pub fn translate_qpath(qpath: rustc_hir::QPath) -> String {
    match qpath {
        rustc_hir::QPath::Resolved(_, path) => segment_to_string(path.segments),
        _ => "".to_string(),
    }
}

pub fn translate_expr(expr: rustc_hir::Expr) -> String {
    match expr.kind {
        rustc_hir::ExprKind::Lit(lit) => translate_lit(&lit.node),
        rustc_hir::ExprKind::Binary(op, e1, e2) => {
            let s1 = translate_expr(*e1);
            let s2 = translate_expr(*e2);
            let ret = format!("{} {} {}", s1, op.node.as_str(), s2);
            ret
        }
        rustc_hir::ExprKind::Call(op, args) => {
            let s1 = translate_expr(*op);
            let s2 = args.iter().map(|arg| translate_expr(*arg));
            let ret = format!("{}({})", s1, s2.collect_vec().join(", "));
            ret
        }
        rustc_hir::ExprKind::Path(qpath) => translate_qpath(qpath),
        rustc_hir::ExprKind::AddrOf(_b, _m, expr) => translate_expr(*expr),
        rustc_hir::ExprKind::Array(exprs) => {
            let ret = format!(
                "[{}]",
                exprs
                    .iter()
                    .map(|expr| translate_expr(*expr))
                    .collect_vec()
                    .join(", ")
            );
            ret
        }
        _ => {
            let ret = format!("<{:#?}>", expr.kind);
            ret
        }
    }
}

pub fn try_to_translate_state_var_info(tcx: TyCtxt, body: rustc_hir::Body) -> Option<String> {
    if let rustc_hir::ExprKind::Call(expr, _) = body.value.kind {
        if let rustc_hir::ExprKind::Path(rustc_hir::QPath::TypeRelative(ty, segment)) = expr.kind {
            let _method = segment.ident;
            let _method_args = segment.args;
            if let TyKind::Path(rustc_hir::QPath::Resolved(_, path)) = ty.kind {
                if let rustc_hir::def::Res::Def(_, def_id) = path.res {
                    let crate_name = tcx.crate_name(def_id.krate);
                    println!("Crate name: {crate_name:#?}");
                    if (crate_name.as_str() != "cw_storage_plus") {
                        return None;
                    }
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

                return Some(segment_to_string(path.segments));
            }
        }
    }
    None
}

pub fn translate_type(ty: rustc_hir::Ty) -> String {
    match ty.kind {
        TyKind::Path(rustc_hir::QPath::Resolved(_, path)) => segment_to_string(path.segments),
        _ => {
            let ret = format!("<{:#?}>", ty.kind);
            ret
        }
    }
}

pub fn translate_fn_decl(decl: rustc_hir::FnDecl, body: rustc_hir::Body) -> String {
    let param_tuples = zip(decl.inputs, body.params);
    let inputs = param_tuples.map(|(input, param)| {
        let translated_param = translate_param(*param);
        let translated_type = translate_type(*input);
        let ret = format!("{}: {}", translated_param, translated_type);
        ret
    });

    let output = match decl.output {
        rustc_hir::FnRetTy::DefaultReturn(_) => "void".to_string(),
        rustc_hir::FnRetTy::Return(ty) => translate_type(*ty),
    };
    let ret = format!("({}): {}", inputs.collect_vec().join(", "), output);
    ret
}
