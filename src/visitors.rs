use itertools::Itertools;
use rustc_hir::intravisit::{self, Visitor};
use rustc_middle::ty::TyCtxt;
use rustc_span::sym::unwrap_or;

use crate::translate::{
    format_fields, get_variant_fields, translate_fn_decl, translate_list,
    try_to_translate_state_var_info, Context, Translatable,
};

pub struct TypeTranslator<'tcx> {
    pub tcx: TyCtxt<'tcx>,
    pub contract_state: Vec<(String, String)>,
    pub ctx: Context,
}

pub struct OpTranslator<'tcx> {
    pub tcx: TyCtxt<'tcx>,
    pub contract_state: Vec<(String, String)>,
    pub ctx: Context,
}

pub fn should_skip(name: &str) -> bool {
    name.starts_with('_')
      || ["", "FIELDS", "VARIANTS"].contains(&name)
      || name.starts_with("query") // skip query functions for now
      || name.starts_with("Query")
      || name.starts_with("ContractError")
      || name.starts_with("get_")
}

impl<'tcx> Visitor<'tcx> for TypeTranslator<'tcx> {
    fn visit_item(&mut self, item: &'tcx rustc_hir::Item<'tcx>) {
        let name = item.ident;
        if should_skip(name.as_str()) {
            return;
        }
        self.ctx.current_item_name = name.as_str().to_string();

        match item.kind {
            rustc_hir::ItemKind::Struct(variant_data, _generics) => {
                let fields = get_variant_fields(&mut self.ctx, variant_data);
                self.ctx
                    .structs
                    .insert(name.as_str().to_string(), fields.clone());
                let formatted_fields = format_fields(fields);
                println!("type {name} = {{ {formatted_fields} }}")
            }

            rustc_hir::ItemKind::Enum(enum_def, _generics) => {
                println!("type {name} =\n{}", enum_def.translate(&mut self.ctx));
            }
            rustc_hir::ItemKind::Fn(sig, _generics, body_id) => {
                let body = self.tcx.hir().body(body_id);
                let (_sig, has_state) = translate_fn_decl(&mut self.ctx, *sig.decl, *body);
                if has_state {
                    self.ctx.stateful_ops.push(name.as_str().to_string())
                }
            }

            _ => {}
        };

        intravisit::walk_item(self, item)
    }
}
impl<'tcx> Visitor<'tcx> for OpTranslator<'tcx> {
    fn visit_item(&mut self, item: &'tcx rustc_hir::Item<'tcx>) {
        let name = item.ident;
        if should_skip(name.as_str()) {
            return;
        }
        self.ctx.current_item_name = name.as_str().to_string();

        match item.kind {
            rustc_hir::ItemKind::Const(_ty, _generics, body) => {
                let const_item = self.tcx.hir().body(body);
                // println!("{const_item:#?}");
                let ret = try_to_translate_state_var_info(&mut self.ctx, self.tcx, *const_item);
                match ret {
                    Some(ret) => {
                        self.contract_state
                            .push((name.as_str().to_string().to_lowercase(), ret));
                    }
                    None => {
                        println!(
                            "pure val {name} = {}",
                            const_item.value.translate(&mut self.ctx)
                        );
                        if !const_item.params.is_empty() {
                            println!("{}", translate_list(const_item.params, &mut self.ctx, ", "))
                        }
                    }
                }
            }

            rustc_hir::ItemKind::Fn(sig, _generics, body_id) => {
                let body = self.tcx.hir().body(body_id);
                let (sig, has_state) = translate_fn_decl(&mut self.ctx, *sig.decl, *body);
                let body_value = body.value.translate(&mut self.ctx);
                let empty_vec = vec![];
                // FIXME: We need to do something about instantiate
                if has_state && name.as_str() != "execute" && name.as_str() != "instantiate" {
                    let (message_type, fields) =
                        match self.ctx.message_type_for_action.get(&name.to_string()) {
                            Some(s) => (
                                s.clone(),
                                self.ctx
                                    .constructors
                                    .get(s)
                                    .map(|x| &x.fields)
                                    .unwrap_or(&empty_vec),
                            ),
                            None => (format!("MessageFor{}", name), &empty_vec),
                        };
                    // println!(
                    //     "[debug] name: {} fields: {:#?}, ctors {:#?}",
                    //     name, fields, self.ctx.constructors
                    // );
                    let values_for_fields = fields
                        .iter()
                        .map(|field| {
                            format!(
                                "nondet {}: {} = {}",
                                field.name, field.ty, field.nondet_value
                            )
                        })
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
                    println!("pure def {name}{sig} = ({body_value}, state)");
                    println!(
                        "action {name}_action = {{
  // TODO: Change next line according to fund expectations
  pure val max_funds = MAX_AMOUNT

  {values_for_fields}
  pure val message = {message_type}{msg_params}

  execute_message(message, max_funds)
}}"
                    )
                } else if name.as_str() == "instantiate" {
                    println!("pure def {name}{sig} = ({body_value}, state)");
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
