use std::{collections::HashMap, fmt::Debug};

use itertools::Itertools;
use rustc_ast::LitKind;
use rustc_hir::{TyKind, VariantData};
use rustc_span::symbol::Ident;
use std::iter::zip;

use crate::{
    state_extraction::try_to_translate_state_var_info,
    types::{Constructor, Context, Field},
};

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

impl Translatable for Ident {
    fn translate(&self, _ctx: &mut Context) -> String {
        let translated_segments: &HashMap<&str, &str> = &HashMap::from([
            ("Vec", "List"),
            ("String", "str"),
            ("Uint128", "int"),
            ("Uint64", "int"),
            ("u128", "int"),
            ("u64", "int"),
            ("Timestamp", "int"),
            ("DepsMut", "ContractState"),
            ("Deps", "ContractState"),
            ("Ok", "Response_Ok"),
            ("deps", "state"),
            ("_deps", "state"),
        ]);

        let s = self.as_str();
        translated_segments.get(s).unwrap_or(&s).to_string()
    }
}

impl Translatable for rustc_hir::PathSegment<'_> {
    fn translate(&self, ctx: &mut Context) -> String {
        let translated = self.ident.translate(ctx);

        match self.args {
            Some(args) => {
                if ["List", "Set"].contains(&translated.as_str()) {
                    // FIXME: this should always happen after type-level polymorphism is implemented
                    let translated_args = translate_list(args.args, ctx, ", ");

                    if translated_args == *"" {
                        return translated.to_string();
                    }

                    return format!("{}[{}]", translated, translated_args);
                }

                translated.to_string()
            }
            None => translated.to_string(),
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
            rustc_hir::PatKind::Struct(qpath, _pat_fields, _) => {
                format!("{}(__r)", qpath.translate(ctx))
            }
            _ => missing_translation(*self, "pattern"),
        }
    }
}

impl Translatable for LitKind {
    fn translate(&self, _ctx: &mut Context) -> String {
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
                if operator == "int_new" {
                    return args[0].translate(ctx);
                }
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

pub fn nondet_value_for_type(ident: String, ty: String) -> String {
    let nondet_values_by_type = HashMap::from([
        ("str", "Set(\"s1\", \"s2\", \"s3\").oneOf()".to_string()),
        ("int", "0.to(MAX_AMOUNT).oneOf()".to_string()),
        // TODO list for other types
    ]);
    if ty == "List[int]" {
        // FIXME make this work with all types
        return format!(
            "
    val possibilities = 1.to(10).map(i => SomeInt(i)).union(Set(NoneInt))
    nondet v1 = possibilities.oneOf()
    nondet v2 = possibilities.oneOf()
    nondet v3 = possibilities.oneOf()
    val {ident}: {ty} = [v1, v2, v3].foldl([], (acc, v) => match v {{
      | SomeInt(i) => acc.append(i)
      | NoneInt => acc
    }})
"
        );
    }
    format!(
        "nondet {}: {} = {}",
        ident,
        ty,
        nondet_values_by_type
            .get(ty.as_str())
            .unwrap_or(&format!("nondet_value_for_type({ty})"))
    )
}

pub fn get_variant_fields(ctx: &mut Context, variant_data: VariantData) -> Vec<Field> {
    variant_data
        .fields()
        .iter()
        .map(|field| {
            let field_ident = field.ident.to_string();
            let field_type = field.ty.translate(ctx);
            Field {
                name: field_ident.clone(),
                ty: field_type.clone(),
                nondet_value: nondet_value_for_type(field_ident.clone(), field_type),
            }
        })
        .collect_vec()
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
            if translated_type == "ContractState" {
                ctx.stateful_ops.push(ctx.current_item_name.clone());
                return "state: ContractState".to_string();
            }
            format!("{}: {}", translated_param, translated_type)
        })
        .collect_vec()
        .join(", ");

    let output = decl.output.translate(ctx);

    // If one of the params is of type Deps or DepsMut, and the return type is "Result", this is a state transformer,
    // and therefore should take the state as an argument and return it
    if input.contains("ContractState") && output.contains("Result") {
        let ret = format!("({input}): ({output}, ContractState)");
        return (ret, true);
    }

    let ret = format!("({}): {}", input, output);
    (ret, false)
}
pub fn should_skip(name: &str) -> bool {
    name.starts_with('_')
      || ["", "FIELDS", "VARIANTS"].contains(&name)
      || name.starts_with("query") // skip query functions for now
      || name.starts_with("Query")
      || name.starts_with("ContractError")
      || name.starts_with("get_")
}

impl Translatable for rustc_hir::Item<'_> {
    fn translate(&self, ctx: &mut Context) -> String {
        let name = self.ident;
        if should_skip(name.as_str()) {
            return "".to_string();
        }
        ctx.current_item_name = name.as_str().to_string();

        match self.kind {
            rustc_hir::ItemKind::Const(_ty, _generics, body) => {
                let const_item = ctx.tcx.hir().body(body);
                match try_to_translate_state_var_info(ctx, *const_item) {
                    Some(ret) => {
                        ctx.contract_state
                            .push((name.as_str().to_string().to_lowercase(), ret));
                        "".to_string()
                    }
                    None => {
                        format!("pure val {name} = {}", const_item.value.translate(ctx))
                        // if !const_item.params.is_empty() {
                        //     format!("{}", translate_list(const_item.params, ctx, ", "))
                        // }
                    }
                }
            }

            rustc_hir::ItemKind::Fn(sig, _generics, body_id) => {
                let body = ctx.tcx.hir().body(body_id);
                let (sig, has_state) = translate_fn_decl(ctx, *sig.decl, *body);

                let body_value = body.value.translate(ctx);
                let empty_vec = vec![];
                // FIXME: We need to do something about instantiate
                if has_state && name.as_str() != "execute" && name.as_str() != "instantiate" {
                    let (message_type, fields) =
                        match ctx.message_type_for_action.get(&name.to_string()) {
                            Some(s) => (
                                s.clone(),
                                ctx.constructors
                                    .get(s)
                                    .map(|x| &x.fields)
                                    .unwrap_or(&empty_vec),
                            ),
                            None => (format!("MessageFor{}", name), &empty_vec),
                        };
                    let values_for_fields = fields
                        .iter()
                        .map(|field| field.nondet_value.clone())
                        .collect_vec()
                        .join("\n  ");
                    let field_params = fields
                        .iter()
                        .map(|field| {
                            let name = field.name.clone();
                            format!("{name}: {name}")
                        })
                        .collect_vec()
                        .join(", ");
                    let msg_params = if fields.is_empty() {
                        "".to_string()
                    } else {
                        format!("({{ {field_params} }})")
                    };
                    format!(
                        "  pure def {name}{sig} = ({body_value}, state)
                            
  action {name}_action = {{
    // TODO: Change next line according to fund expectations
    pure val max_funds = MAX_AMOUNT
  
    {values_for_fields}
    pure val message = {message_type}{msg_params}
  
    execute_message(message, max_funds)
  }}"
                    )
                } else if name.as_str() == "instantiate" {
                    format!("pure def {name}{sig} = ({body_value}, state)")
                } else {
                    format!("pure def {name}{sig} = {body_value}")
                }
            }
            rustc_hir::ItemKind::Struct(variant_data, _generics) => {
                let fields = get_variant_fields(ctx, variant_data);
                ctx.structs
                    .insert(name.as_str().to_string(), fields.clone());
                let formatted_fields = format_fields(fields);
                format!("type {name} = {{ {formatted_fields} }}")
            }

            rustc_hir::ItemKind::Enum(enum_def, _generics) => {
                format!("type {name} =\n{}", enum_def.translate(ctx))
            }
            // Safely ignore import-related things
            rustc_hir::ItemKind::Mod(_) => "".to_string(),
            rustc_hir::ItemKind::Use(_, _) => "".to_string(),
            rustc_hir::ItemKind::ExternCrate(_) => "".to_string(),
            _ => missing_translation(*self, "item"),
        }
    }
}
