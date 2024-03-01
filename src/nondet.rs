use itertools::Itertools;

use crate::translate::Translatable;
use crate::types::{Constructor, Context, Field};
use std::fmt::Debug;
use std::iter::zip;

pub type NondetInfo = (Vec<String>, String);

/// Trait for generating a nondeterministic value for something.
pub trait NondetValue: Translatable {
    fn nondet_info(&self, ctx: &mut Context, ident: &str) -> NondetInfo;
    fn nondet_definition(&self, ctx: &mut Context, ident: &str) -> String {
        let info = self.nondet_info(ctx, ident);
        let ty = self.translate(ctx);
        nondet_info_to_def(info, &ty, ident)
    }
}

fn nondet_info_to_def(info: NondetInfo, ty: &str, ident: &str) -> String {
    let (auxiliary_defs, value) = info;
    if auxiliary_defs.is_empty() {
        format!("nondet {}: {} = {}.oneOf()", ident, ty, value)
    } else {
        format!(
            "{}\n    pure val {}: {} = {}",
            auxiliary_defs.join("\n    "),
            ident,
            ty,
            value
        )
    }
}

impl NondetValue for Vec<Field> {
    fn nondet_info(&self, ctx: &mut Context, ident: &str) -> NondetInfo {
        // Each field needs its own named nondet value, in the form of
        // `nondet field_name: field_type = something.oneOf()`
        let defs = self
            .iter()
            .map(|field| {
                nondet_info_to_def(
                    field.nondet_info.clone(),
                    &field.ty,
                    format!("{}_{}", ident, field.name).as_str(),
                )
            })
            .collect_vec();

        // And these values should be assembled into a record to be given to the constructor
        let record_fields = self
            .iter()
            .map(|field| format!("{name}: {ident}_{name}", name = field.name.clone()))
            .collect_vec()
            .join(", ");

        let record = format!("{{ {} }}", record_fields);

        (defs, record)
    }
}

impl NondetValue for Constructor {
    fn nondet_info(&self, ctx: &mut Context, ident: &str) -> NondetInfo {
        if self.fields.is_empty() {
            // If the constuctor has no field, it can only contain one value, and that is deterministic
            return (vec![], self.name.clone());
        }

        let (defs, record) = self.fields.nondet_info(ctx, ident);
        let constructor = format!("{}({})", self.name, record);

        (defs, constructor)
    }
}

impl NondetValue for rustc_hir::PathSegment<'_> {
    fn nondet_info(&self, ctx: &mut Context, ident: &str) -> NondetInfo {
        let translated_type = self.ident.translate(ctx);
        let args_as_type = match self.args {
            Some(args) => match args.args {
                [rustc_hir::GenericArg::Type(ty)] => Some(ty),
                _ => None,
            },
            None => None,
        };

        if let Some(ty) = args_as_type {
            if translated_type == "List" {
                return nondet_value_for_list(ty, ctx, ident);
            }
            if translated_type == "Option" {
                return nondet_value_for_option(ty, ctx, ident);
            }
        }

        if ctx.constructors.contains_key(&translated_type) {
            return ctx.constructors[&translated_type]
                .clone()
                .nondet_info(ctx, ident);
        }

        eprintln!("{translated_type}");
        if ctx.structs.contains_key(&translated_type) {
            eprintln!("Struct: {translated_type}");
            let fields = ctx.structs[&translated_type].clone();
            return fields.nondet_info(ctx, ident);
        }
        let value = match translated_type.as_str() {
            "str" | "Addr" => "Set(\"s1\", \"s2\", \"s3\")".to_string(),
            "int" => "0.to(MAX_AMOUNT)".to_string(),
            "bool" => "Bool".to_string(),
            _ => missing_nondet_value(*self, ctx, "path segment"),
        };

        (vec![], value)
    }
}

fn nondet_list<T: NondetValue>(
    items: &[T],
    ctx: &mut Context,
    ident: &str,
) -> (Vec<String>, Vec<String>) {
    if items.len() == 1 {
        let (defs, value) = items[0].nondet_info(ctx, ident);
        return (defs, vec![value]);
    }
    let declarations = items
        .iter()
        .enumerate()
        .map(|(i, item)| item.nondet_definition(ctx, format!("{}_{}", ident, i).as_str()))
        .collect_vec();

    let values = items
        .iter()
        .enumerate()
        .map(|(i, _)| format!("{}_{}", ident, i))
        .collect_vec();

    (declarations, values)
}

fn nondet_vec<T: NondetValue>(
    items: Vec<T>,
    ctx: &mut Context,
    ident: &str,
) -> (Vec<String>, Vec<String>) {
    if items.len() == 1 {
        let (defs, value) = items[0].nondet_info(ctx, ident);
        return (defs, vec![value]);
    }
    let declarations = items
        .iter()
        .enumerate()
        .map(|(i, item)| item.nondet_definition(ctx, format!("{}_{}", ident, i).as_str()))
        .collect_vec();

    let values = items
        .iter()
        .enumerate()
        .map(|(i, _)| format!("{}_{}", ident, i))
        .collect_vec();

    (declarations, values)
}

fn missing_nondet_value<T: NondetValue + Debug + Translatable>(
    item: T,
    ctx: &mut Context,
    kind: &str,
) -> String {
    eprintln!("No nondet value for {}: {:?}", kind, item.translate(ctx));
    "<missing-nondet-value>".to_string()
}

fn nondet_value_for_list(ty: &rustc_hir::Ty<'_>, ctx: &mut Context, ident: &str) -> NondetInfo {
    let (element_defs, element_value) = ty.nondet_info(ctx, ident);
    if !element_defs.is_empty() {
        // we might be able to do something, but will it work for apalache?
        eprintln!(
            "Too complex list type: {}. Generating simple list with 1 element.",
            ty.translate(ctx)
        );
        return (element_defs, format!("[{element_value}]"));
    }

    let defs = vec![
        format!(
            "val possibilities = {}.map(i => Some(i)).union(Set(None))",
            element_value
        ),
        "nondet v1 = possibilities.oneOf()".to_string(),
        "nondet v2 = possibilities.oneOf()".to_string(),
        "nondet v3 = possibilities.oneOf()".to_string(),
    ];

    let value = "
    [v1, v2, v3].foldl([], (acc, v) => match v {
      | Some(i) => acc.append(i)
      | None => acc
    })
"
    .to_string();

    (defs, value)
}

fn nondet_value_for_option(ty: &rustc_hir::Ty<'_>, ctx: &mut Context, ident: &str) -> NondetInfo {
    let (element_defs, element_value) = ty.nondet_info(ctx, ident);
    if !element_defs.is_empty() {
        // we might be able to do something, but will it work for apalache?
        eprintln!(
            "Too complex list type: {}. Generating Some() case only",
            ty.translate(ctx)
        );
        return (element_defs, format!("Some({element_value})"));
    }

    let element_ident = format!("{}_element", ident);
    let defs = vec![ty.nondet_definition(ctx, element_ident.as_str())];
    let value = format!("Set(Some({element_ident}), None).oneOf()");

    (defs, value)
}

impl NondetValue for rustc_hir::Ty<'_> {
    fn nondet_info(&self, ctx: &mut Context, ident: &str) -> NondetInfo {
        match self.kind {
            rustc_hir::TyKind::Path(qpath) => qpath.nondet_info(ctx, ident),
            rustc_hir::TyKind::Tup(tys) => {
                let (defs, values) = nondet_list(tys, ctx, ident);
                let tuple = format!("({})", values.join(", "));
                (defs, tuple)
            }
            rustc_hir::TyKind::Array(ty, _) | rustc_hir::TyKind::Slice(ty) => {
                nondet_value_for_list(ty, ctx, ident)
            }
            _ => (vec![], missing_nondet_value(*self, ctx, "type")),
        }
    }
}

impl NondetValue for rustc_hir::QPath<'_> {
    fn nondet_info(&self, ctx: &mut Context, ident: &str) -> NondetInfo {
        match self {
            rustc_hir::QPath::Resolved(_, path) => {
                // This is probably wrong
                let (defs, values) = nondet_list(path.segments, ctx, ident);
                eprintln!("QPath {}, defs: {defs:#?}, values: {}", self.translate(ctx), values.join(", "));
                (defs, values.join(", "))
            }
            // rustc_hir::QPath::TypeRelative(ty, segment) => {
            //     format!("{}_{}", ty.translate(ctx), segment.translate(ctx))
            // }
            _ => (vec![], missing_nondet_value(*self, ctx, "qualified-path")),
        }
    }
}
