use std::collections::HashMap;

use itertools::Itertools;
use rustc_ast::LitKind;
use rustc_hir::{TyKind, VariantData};
use rustc_middle::ty::TyCtxt;
use std::iter::zip;

pub fn segment_to_string(segments: &[rustc_hir::PathSegment], sep: &str) -> String {
    // let sep = optional_sep.unwrap_or("::".to_string()).as_str();

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
    a.join(sep)

    // TODO: convert arguments
    // let method_args = segment.args;
}

pub fn translate_param(param: rustc_hir::Param) -> String {
    translate_pat(*param.pat).0
}

pub fn translate_pat(pat: rustc_hir::Pat) -> (String, Vec<String>) {
    match pat.kind {
        rustc_hir::PatKind::Binding(_a, _id, name, _n) => (name.as_str().to_string(), vec![]),
        rustc_hir::PatKind::Path(qpath) => (translate_qpath(qpath), vec![]),
        rustc_hir::PatKind::Struct(qpath, pat_fields, _) => {
            let fields = pat_fields.iter().map(|field| {
                field.ident.to_string()
                // TODO: raise error if pat != field (I think this is matching for {field_ident: field_pat})
                // let field_pat = translate_pat(*field.pat);
            });
            let ret = format!("{}(__r)", translate_qpath(qpath),);
            (ret, fields.collect_vec())
        }
        _ => {
            // let ret = format!("pat<{:#?}>", pat.kind);
            // ret
            ("".to_string(), vec![])
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
        rustc_hir::QPath::Resolved(_, path) => segment_to_string(path.segments, "_"),
        _ => "".to_string(),
    }
}

pub fn translate_expr(expr: rustc_hir::Expr, record_fields: &Vec<String>) -> String {
    match expr.kind {
        rustc_hir::ExprKind::Lit(lit) => translate_lit(&lit.node),
        rustc_hir::ExprKind::Binary(op, e1, e2) => {
            let s1 = translate_expr(*e1, record_fields);
            let s2 = translate_expr(*e2, record_fields);
            let ret = format!("{} {} {}", s1, op.node.as_str(), s2);
            ret
        }
        rustc_hir::ExprKind::Call(op, args) => {
            let s1 = translate_expr(*op, record_fields);
            let s2 = args.iter().map(|arg| translate_expr(*arg, record_fields));
            let ret = format!("{}({})", s1, s2.collect_vec().join(", "));
            ret
        }
        rustc_hir::ExprKind::Path(qpath) => {
            let name = translate_qpath(qpath);
            if record_fields.contains(&name) {
                let ret = format!("__r.{}", name);
                ret
            } else {
                name
            }
        }
        rustc_hir::ExprKind::AddrOf(_b, _m, expr) => translate_expr(*expr, record_fields),
        rustc_hir::ExprKind::Array(exprs) => {
            let ret = format!(
                "[{}]",
                exprs
                    .iter()
                    .map(|expr| translate_expr(*expr, record_fields))
                    .collect_vec()
                    .join(", ")
            );
            ret
        }
        rustc_hir::ExprKind::Block(block, _label) => match block.expr {
            Some(expr) => {
                let ret = format!("{{ {} }}", translate_expr(*expr, record_fields));
                ret
            }
            None => "".to_string(),
        },
        rustc_hir::ExprKind::Match(expr, arm, source) => {
            let ret = format!(
                "match {} {{\n{}}}",
                translate_expr(*expr, record_fields),
                arm.iter()
                    .map(|arm| {
                        let (pat, fields) = translate_pat(*arm.pat);
                        let expr = translate_expr(*arm.body, &fields);
                        format!("  | {} => {}", pat, expr)
                    })
                    .collect_vec()
                    .join("\n")
            );
            ret
        }

        _ => {
            // let ret = format!("<{:?}>", expr.kind);
            // ret
            "".to_string()
        }
    }
}

fn translate_generic_arg(arg: rustc_middle::ty::GenericArg) -> String {
    let kind = arg.unpack();
    match kind {
        rustc_middle::ty::GenericArgKind::Type(ty) => {
            let ret = format!("{:#?}", ty);
            // FIXME: This should be quite unstable, but I couldn't figure out how to navigate `ty` here
            ret.split("::")
                .last()
                .unwrap()
                .split(' ')
                .last()
                .unwrap()
                .split(')')
                .next()
                .unwrap()
                .to_string()
        }
        _ => {
            let ret = format!("<{:#?}>", kind);
            ret
        }
    }
}

pub fn try_to_translate_state_var_info(tcx: TyCtxt, body: rustc_hir::Body) -> Option<String> {
    if let rustc_hir::ExprKind::Call(expr, _) = body.value.kind {
        if let rustc_hir::ExprKind::Path(rustc_hir::QPath::TypeRelative(ty, segment)) = expr.kind {
            // let method = segment.ident;
            // let method_args = segment.args;
            // println!("Method: {method} ({method_args:#?})");
            if let TyKind::Path(rustc_hir::QPath::Resolved(_, path)) = ty.kind {
                if let rustc_hir::def::Res::Def(kind, def_id) = path.res {
                    let crate_name = tcx.crate_name(def_id.krate);
                    if (crate_name.as_str() != "cw_storage_plus") {
                        return None;
                    }

                    // if crate name is `cw_storage_plus`, we need to translate
                    // this into part of the contract state
                    // ------------

                    let def_id = expr.hir_id.owner.def_id; // def_id identifies the main function

                    let ty = tcx.typeck(def_id).node_type(expr.hir_id);
                    if let rustc_type_ir::TyKind::FnDef(ty_def_id, generic_args) = ty.kind() {
                        // We need to look into the GenericArgs here (from FnDef)
                        // to annotate the quint state variable properly

                        let translated_types = generic_args
                            .iter()
                            .map(|t| translate_generic_arg(t))
                            .collect_vec();
                        // segment_to_string(path.segments);

                        // For Map::new, the 2nd and 3rd generics are the map types
                        // For Item::new, the 2nd generic is the type
                        // Are there other options?
                        let translated_type =
                            if (path.segments[0].ident.as_str().starts_with("Map")) {
                                let ret = format!(
                                    "Map<{}, {}>",
                                    translated_types[1], translated_types[2]
                                );
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

pub fn translate_type(ty: rustc_hir::Ty) -> String {
    match ty.kind {
        TyKind::Path(rustc_hir::QPath::Resolved(_, path)) => segment_to_string(path.segments, "_"),
        _ => {
            let ret = format!("<{:#?}>", ty.kind);
            ret
        }
    }
}

pub fn translate_fn_decl(decl: rustc_hir::FnDecl, body: rustc_hir::Body) -> (String, bool) {
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

    let input = inputs.collect_vec().join(", ");

    // If one of the params is of type Deps or DepsMut, and the return type is "Result", this is a state transformer,
    // and therefore should take the state as an argument and return it
    if input.contains("Deps") && output.contains("Result") {
        let ret = format!("(contract_state: ContractState, {input}): ({output}, ContractState)");
        return (ret, true);
    }

    let ret = format!("({}): {}", input, output);
    (ret, false)
}
