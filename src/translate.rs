use std::{collections::HashMap, fmt::Debug};

use itertools::Itertools;
use rustc_ast::LitKind;
use rustc_hir::{TyKind, VariantData};
use rustc_span::symbol::Ident;
use std::iter::zip;

use crate::{
    nondet::NondetValue,
    state_extraction::try_to_translate_state_var_info,
    types::{fallback_constructor, Constructor, Context, Field, Function},
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

pub fn translate_vec<T: Translatable>(items: Vec<T>, ctx: &mut Context, sep: &str) -> String {
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
        let param = self.pat.translate(ctx);
        ctx.pat_fields.clear();
        param
    }
}

impl Translatable for rustc_hir::Pat<'_> {
    fn translate(&self, ctx: &mut Context) -> String {
        match self.kind {
            rustc_hir::PatKind::Binding(_a, _id, name, _n) => name.as_str().to_string(),
            rustc_hir::PatKind::Path(qpath) => qpath.translate(ctx),
            rustc_hir::PatKind::Struct(qpath, pat_fields, _) => {
                // TODO: raise error if pat != field (I think this is matching for {field_ident: field_pat})
                // let field_pat = translate_pat(*field.pat);

                let fields = pat_fields
                    .iter()
                    .map(|field| field.ident.to_string())
                    .collect_vec();
                ctx.pat_fields.extend(fields.clone());

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
                    "match {} {{\n{}\n  }}",
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
        let fields = ctx.pat_fields.clone();
        ctx.pat_fields.clear();
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
        format!("    | {} => {}", pat, expr)
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

impl Translatable for VariantData<'_> {
    fn translate(&self, ctx: &mut Context) -> String {
        let fields = self
            .fields()
            .iter()
            .map(|field| {
                let field_ident = field.ident.to_string();
                Field {
                    name: field_ident.clone(),
                    ty: field.ty.translate(ctx),
                    nondet_value: field.ty.nondet_value(ctx, &field_ident),
                }
            })
            .collect_vec();

        ctx.struct_fields.extend(fields.clone());

        translate_vec(fields, ctx, ", ")
    }
}

impl Translatable for Field {
    fn translate(&self, _ctx: &mut Context) -> String {
        format!("{}: {}", self.name, self.ty)
    }
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
        let translated_fields = self.data.translate(ctx);

        let fields = ctx.struct_fields.clone();
        ctx.struct_fields.clear();

        ctx.constructors.insert(
            qualified_ident.clone(),
            Constructor {
                name: qualified_ident.clone(),
                fields,
            },
        );

        if self.data.fields().is_empty() {
            format!("    | {qualified_ident}")
        } else {
            format!("    | {qualified_ident}({{ {translated_fields} }})",)
        }
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

impl Translatable for Function<'_> {
    fn translate(&self, ctx: &mut Context) -> String {
        // If one of the params is of type Deps or DepsMut, and the return type is "Result", this is a state transformer,
        // and therefore should take the state as an argument and return it
        let mut has_state = false;

        let param_tuples = zip(self.decl.inputs, self.body.params);
        let input = param_tuples
            .map(|(input, param)| {
                let translated_param = param.translate(ctx);
                let translated_type = input.translate(ctx);
                if translated_type == "ContractState" {
                    has_state = true;
                    return "state: ContractState".to_string();
                }
                format!("{}: {}", translated_param, translated_type)
            })
            .collect_vec()
            .join(", ");

        let output = self.decl.output.translate(ctx);

        if has_state {
            ctx.stateful_ops.push(ctx.current_item_name.clone());

            format!("({input}): ({output}, ContractState)")
        } else {
            format!("({}): {}", input, output)
        }
    }
}

impl Translatable for rustc_hir::Item<'_> {
    fn translate(&self, ctx: &mut Context) -> String {
        let name = self.ident.as_str();

        if name.starts_with('_')
          || ["", "FIELDS", "VARIANTS"].contains(&name)
          || name.starts_with("query") // skip query functions for now
          || name.starts_with("Query")
          || name.starts_with("ContractError")
          || name.starts_with("get_")
        {
            // skip irrelevant items
            return "".to_string();
        }

        ctx.current_item_name = name.to_string();

        match self.kind {
            rustc_hir::ItemKind::Const(_ty, _generics, body) => {
                let const_item = ctx.tcx.hir().body(body);
                match try_to_translate_state_var_info(ctx, *const_item) {
                    Some(ret) => {
                        ctx.contract_state
                            .push((name.to_string().to_lowercase(), ret));
                        "".to_string()
                    }
                    None => {
                        format!("  pure val {name} = {}", const_item.value.translate(ctx))
                        // if !const_item.params.is_empty() {
                        //     format!("{}", translate_list(const_item.params, ctx, ", "))
                        // }
                    }
                }
            }

            rustc_hir::ItemKind::Fn(sig, _generics, body_id) => {
                let body = ctx.tcx.hir().body(body_id);
                let function = Function {
                    decl: *sig.decl,
                    body: *body,
                };
                let sig = function.translate(ctx);
                let has_state = ctx.stateful_ops.contains(&name.to_string());
                let body_value = body.value.translate(ctx);

                if !has_state || name == "execute" {
                    // Direct translation for non-stateful functions (i.e.
                    // helpers) Also for execute, since it's a special case - it
                    // has a match statement to call other actions. Excute is
                    // always called from `execute_message` from the boilerplate
                    // part
                    return format!("  pure def {name}{sig} = {body_value}");
                }

                if name == "instantiate" {
                    // FIXME: We need to do something about instantiate
                    // Instantiate is a stateful function (taking state as an
                    // argument and returning it) But currently we don't call it
                    // in the state machine (from `step`). We probably need to
                    // update the boilerplate stuff to call it.
                    return format!("  pure def {name}{sig} = ({body_value}, state)");
                }

                let ctor: Constructor = ctx
                    .message_type_for_action
                    .get(&name.to_string())
                    .and_then(|s| ctx.constructors.get(s).cloned())
                    .unwrap_or_else(|| fallback_constructor(name));

                let nondet_value = ctor.nondet_value(ctx, "message");

                format!(
                    "  pure def {name}{sig} = ({body_value}, state)
                            
  action {name}_action = {{
    // TODO: Change next line according to fund expectations
    pure val max_funds = MAX_AMOUNT
    {nondet_value}
    execute_message(message, max_funds)
  }}"
                )
            }
            rustc_hir::ItemKind::Struct(variant_data, _generics) => {
                let translated_fields = variant_data.translate(ctx);
                let fields = ctx.struct_fields.clone();
                ctx.struct_fields.clear();

                ctx.structs.insert(name.to_string(), fields);

                format!("  type {name} = {{ {translated_fields} }}")
            }

            rustc_hir::ItemKind::Enum(enum_def, _generics) => {
                format!("  type {name} =\n{}", enum_def.translate(ctx))
            }
            // Safely ignore import-related things
            rustc_hir::ItemKind::Mod(..) => "".to_string(),
            rustc_hir::ItemKind::Use(..) => "".to_string(),
            rustc_hir::ItemKind::ExternCrate(..) => "".to_string(),
            _ => missing_translation(*self, "item"),
        }
    }
}
