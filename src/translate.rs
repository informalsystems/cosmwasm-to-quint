use std::{collections::HashMap, fmt::Debug};

use itertools::Itertools;
use rustc_ast::LitKind;
use rustc_hir::{TyKind, VariantData};
use rustc_middle::ty::TyCtxt;
use std::iter::zip;

pub trait Translatable {
    fn translate(&self, ctx: &mut Context) -> String;
}

pub fn translate_list<T: Translatable>(items: &[T], ctx: &mut Context, sep: &str) -> String {
    items
        .iter()
        .map(|x| x.translate(ctx))
        .collect_vec()
        .join(sep)
}

pub fn missing_translation<T: Translatable + Debug>(item: T, descr: &str) -> String {
    eprintln!("No translation for {descr}: {:#?}", item);
    format!("<missing-{descr}>")
}

impl Translatable for rustc_hir::PathSegment<'_> {
    fn translate(&self, ctx: &mut Context) -> String {
        let translated_segments: HashMap<&str, &str> = HashMap::from([
            ("Vec", "List"),
            ("String", "str"),
            ("Uint128", "int"),
            ("Uint64", "int"),
            ("u128", "int"),
            ("u64", "int"),
            ("Timestamp", "int"),
            ("DepsMut", "Deps"),
            ("Ok", "Response_Ok"),
        ]);

        let s = self.ident.as_str();
        let translated = translated_segments.get(s);
        match translated {
            Some(t) => match self.args {
                Some(args) => {
                    let translated_args = translate_list(args.args, ctx, ", ");

                    if translated_args == *"" {
                        return t.to_string();
                    }

                    format!("{}[{}]", t, translated_args)
                }
                None => t.to_string(),
            },
            None => s.to_string(),
        }
    }
}

impl Translatable for rustc_hir::QPath<'_> {
    fn translate(&self, ctx: &mut Context) -> String {
        match self {
            rustc_hir::QPath::Resolved(_, path) => translate_list(path.segments, ctx, "_"),
            rustc_hir::QPath::TypeRelative(ty, segment) => {
                format!("{}_{}", ty.translate(ctx), segment.translate(ctx))
            }
            _ => missing_translation(*self, "qualified-path"),
        }
    }
}

impl Translatable for rustc_hir::Ty<'_> {
    fn translate(&self, ctx: &mut Context) -> String {
        match self.kind {
            TyKind::Path(qpath) => qpath.translate(ctx),
            TyKind::Tup(tys) => {
                format!("({})", translate_list(tys, ctx, ", "))
            }
            _ => missing_translation(*self, "type"),
        }
    }
}

impl Translatable for rustc_hir::Param<'_> {
    fn translate(&self, ctx: &mut Context) -> String {
        self.pat.translate(ctx)
    }
}

impl Translatable for rustc_hir::Pat<'_> {
    fn translate(&self, ctx: &mut Context) -> String {
        match self.kind {
            rustc_hir::PatKind::Binding(_a, _id, name, _n) => name.as_str().to_string(),
            rustc_hir::PatKind::Path(qpath) => qpath.translate(ctx),
            rustc_hir::PatKind::Struct(qpath, pat_fields, _) => {
                format!("{}(__r)", qpath.translate(ctx))
            }
            _ => missing_translation(*self, "pattern"),
        }
    }
}

impl Translatable for LitKind {
    fn translate(&self, ctx: &mut Context) -> String {
        match self {
            LitKind::Str(sym, rustc_ast::StrStyle::Cooked) => {
                let ret = format!("\"{}\"", sym.as_str());
                ret
            }
            LitKind::Int(i, _) => i.to_string(),
            _ => missing_translation(self.clone(), "literal"),
        }
    }
}

impl Translatable for rustc_hir::Expr<'_> {
    fn translate(&self, ctx: &mut Context) -> String {
        match self.kind {
            rustc_hir::ExprKind::Lit(lit) => lit.node.translate(ctx),
            rustc_hir::ExprKind::Binary(op, e1, e2) => {
                format!(
                    "{} {} {}",
                    e1.translate(ctx),
                    op.node.as_str(),
                    e2.translate(ctx)
                )
            }
            rustc_hir::ExprKind::Call(op, args) => {
                let operator = op.translate(ctx);
                let arguments = translate_list(args, ctx, ", ");
                if ctx.stateful_ops.contains(&operator) {
                    // If this is a stateful operation, we need to add the state as the first argument
                    return format!("{}(state, {})", operator, arguments);
                }
                if arguments.is_empty() {
                    // Quint doesn't have nullary operators
                    return operator;
                }
                format!("{}({})", operator, arguments)
            }
            rustc_hir::ExprKind::Path(qpath) => {
                let name = qpath.translate(ctx);
                if ctx.record_fields.contains(&name) {
                    format!("__r.{}", name)
                } else {
                    name
                }
            }
            rustc_hir::ExprKind::AddrOf(_b, _m, expr) => expr.translate(ctx),
            rustc_hir::ExprKind::Array(exprs) => format!("[{}]", translate_list(exprs, ctx, ", ")),
            rustc_hir::ExprKind::Block(block, _label) => match block.expr {
                Some(expr) => expr.translate(ctx),
                None => "".to_string(),
            },
            rustc_hir::ExprKind::Match(expr, arm, _source) => {
                let match_expr = expr.translate(ctx);
                if match_expr != "msg" {
                    // FIXME: should probably look into type for ExecuteMsg type
                    return "Response_Ok(Response_new)".to_string();
                }

                format!(
                    "match {} {{\n{}}}",
                    match_expr,
                    translate_list(arm, ctx, "\n")
                )
            }
            rustc_hir::ExprKind::MethodCall(_, expr, _, _) => expr.translate(ctx),
            rustc_hir::ExprKind::DropTemps(expr) => expr.translate(ctx),
            rustc_hir::ExprKind::Field(expr, field) => format!("{}.{}", expr.translate(ctx), field),
            _ => missing_translation(*self, "expression"),
        }
    }
}

impl Translatable for rustc_hir::Arm<'_> {
    fn translate(&self, ctx: &mut Context) -> String {
        let pat = self.pat.translate(ctx);
        let fields = get_pat_fields(*self.pat);
        // Put fields in context so they are translated as __r.field
        ctx.record_fields.extend(fields.clone());
        let expr = self.body.translate(ctx);
        // Remove fields from context
        fields.iter().for_each(|field| {
            ctx.record_fields.remove(
                ctx.record_fields
                    .iter()
                    .position(|x| *x == *field)
                    .expect("field not found"),
            );
        });

        ctx.message_type_for_action.insert(
            expr.clone().split('(').next().unwrap().to_string(),
            pat.clone().split('(').next().unwrap().to_string(),
        );
        format!("  | {} => {}", pat, expr)
    }
}

impl Translatable for rustc_hir::GenericArg<'_> {
    fn translate(&self, ctx: &mut Context) -> String {
        match self {
            rustc_hir::GenericArg::Type(ty) => ty.translate(ctx),
            rustc_hir::GenericArg::Lifetime(_lt) => "".to_string(),
            _ => missing_translation(*self, "generic-arg"),
        }
    }
}

impl Translatable for rustc_middle::ty::GenericArg<'_> {
    fn translate(&self, _ctx: &mut Context) -> String {
        let kind = self.unpack();
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
            rustc_middle::ty::GenericArgKind::Const(_) => "".to_string(),
            rustc_middle::ty::GenericArgKind::Lifetime(_) => "".to_string(),
        }
    }
}

fn get_pat_fields(pat: rustc_hir::Pat) -> Vec<String> {
    match pat.kind {
        // TODO: raise error if pat != field (I think this is matching for {field_ident: field_pat})
        // let field_pat = translate_pat(*field.pat);
        rustc_hir::PatKind::Struct(_, pat_fields, _) => pat_fields
            .iter()
            .map(|field| field.ident.to_string())
            .collect_vec(),
        _ => vec![],
    }
}

pub fn get_variant_fields(ctx: &mut Context, variant_data: VariantData) -> Vec<Field> {
    let nondet_value_for_type = HashMap::from([
        ("str", "Set(\"s1\", \"s2\", \"s3\").oneOf()".to_string()),
        ("int", "0.to(MAX_AMOUNT).oneOf()".to_string()),
        // TODO list -> possibilities
    ]);
    variant_data
        .fields()
        .iter()
        .map(|field| {
            let field_ident = field.ident.to_string();
            let field_type = field.ty.translate(ctx);
            Field {
                name: field_ident,
                ty: field_type.clone(),
                nondet_value: nondet_value_for_type
                    .get(&field_type.clone().as_str())
                    .map(|x| x.to_string())
                    .unwrap_or(format!("nondet_value_for_type({})", field_type)),
            }
        })
        .collect_vec()
}

#[derive(Clone, Debug)]
pub struct Field {
    pub name: String,
    pub ty: String,
    pub nondet_value: String,
}

#[derive(Clone, Debug)]
pub struct Constructor {
    pub name: String,
    pub fields: Vec<Field>,
}

#[derive(Clone, Debug)]
pub struct Context {
    pub message_type_for_action: HashMap<String, String>,
    pub constructors: HashMap<String, Constructor>,
    pub stateful_ops: Vec<String>,
    pub structs: HashMap<String, Vec<Field>>,
    pub record_fields: Vec<String>,
    pub current_item_name: String,
}

pub fn try_to_translate_state_var_info(
    ctx: &mut Context,
    tcx: TyCtxt,
    body: rustc_hir::Body,
) -> Option<String> {
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

                        let translated_types =
                            generic_args.iter().map(|t| t.translate(ctx)).collect_vec();
                        // segment_to_string(path.segments);

                        // For Map::new, the 2nd and 3rd generics are the map types
                        // For Item::new, the 2nd generic is the type
                        // Are there other options?
                        let translated_type =
                            if (path.segments[0].ident.as_str().starts_with("Map")) {
                                let ret =
                                    format!("{} -> {}", translated_types[1], translated_types[2]);
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

pub fn format_fields(fields: Vec<Field>) -> String {
    fields
        .iter()
        .map(|x| format!("{}: {}", x.name, x.ty))
        .collect_vec()
        .join(", ")
}

impl Translatable for rustc_hir::EnumDef<'_> {
    fn translate(&self, ctx: &mut Context) -> String {
        translate_list(self.variants, ctx, "\n")
    }
}

impl Translatable for rustc_hir::Variant<'_> {
    fn translate(&self, ctx: &mut Context) -> String {
        let ident = self.ident;
        let qualified_ident = format!("{}_{ident}", ctx.current_item_name);
        if self.data.fields().is_empty() {
            return format!("  | {qualified_ident}");
        }
        let fields = get_variant_fields(ctx, self.data);
        ctx.constructors.insert(
            qualified_ident.clone(),
            Constructor {
                name: qualified_ident.clone(),
                fields: fields.clone(),
            },
        );
        format!("  | {qualified_ident}({{ {} }})", format_fields(fields))
    }
}

impl Translatable for rustc_hir::FnRetTy<'_> {
    fn translate(&self, ctx: &mut Context) -> String {
        match self {
            rustc_hir::FnRetTy::DefaultReturn(_) => "void".to_string(),
            rustc_hir::FnRetTy::Return(ty) => ty.translate(ctx),
        }
    }
}

pub fn translate_fn_decl(
    ctx: &mut Context,
    decl: rustc_hir::FnDecl,
    body: rustc_hir::Body,
) -> (String, bool) {
    let param_tuples = zip(decl.inputs, body.params);
    let input = param_tuples
        .map(|(input, param)| {
            let translated_param = param.translate(ctx);
            let translated_type = input.translate(ctx);
            format!("{}: {}", translated_param, translated_type)
        })
        .collect_vec()
        .join(", ");

    let output = decl.output.translate(ctx);

    // If one of the params is of type Deps or DepsMut, and the return type is "Result", this is a state transformer,
    // and therefore should take the state as an argument and return it
    if input.contains("Deps") && output.contains("Result") {
        let ret = format!("(state: ContractState, {input}): ({output}, ContractState)");
        return (ret, true);
    }

    let ret = format!("({}): {}", input, output);
    (ret, false)
}
