use itertools::Itertools;
use rustc_hir::intravisit::{self, Visitor};
use rustc_middle::ty::TyCtxt;
use rustc_span::sym::unwrap_or;

use crate::translate::{
    format_fields, translate_enum, translate_expr, translate_fn_decl, translate_variant_data,
    try_to_translate_state_var_info, Context,
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

        match item.kind {
            rustc_hir::ItemKind::Struct(variant_data, _generics) => {
                // println!("Found a struct: {variant_data:#?}");
                let fields = format_fields(translate_variant_data(variant_data));
                println!("type {name} = {{ {fields} }}")
            }

            rustc_hir::ItemKind::Enum(enum_def, _generics) => {
                //  println!("Found an enum: {enum_def:#?}");
                let variants = translate_enum(&mut self.ctx, name.as_str().to_string(), enum_def);
                println!("type {name} =\n{variants}")
            }
            rustc_hir::ItemKind::Fn(sig, _generics, body_id) => {
                let body = self.tcx.hir().body(body_id);
                let (_sig, has_state) = translate_fn_decl(*sig.decl, *body);
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

        match item.kind {
            rustc_hir::ItemKind::Const(_ty, _generics, body) => {
                let const_item = self.tcx.hir().body(body);
                // println!("{const_item:#?}");
                let ret = try_to_translate_state_var_info(self.tcx, *const_item);
                match ret {
                    Some(ret) => {
                        self.contract_state
                            .push((name.as_str().to_string().to_lowercase(), ret));
                    }
                    None => {
                        let ret2 = translate_expr(&mut self.ctx, *const_item.value, &vec![]);
                        println!("pure val {name} = {ret2}");
                        let ret3 = const_item.params;
                        if !ret3.is_empty() {
                            println!("{ret3:#?}")
                        }
                    }
                }
            }

            rustc_hir::ItemKind::Fn(sig, _generics, body_id) => {
                let body = self.tcx.hir().body(body_id);
                let (sig, has_state) = translate_fn_decl(*sig.decl, *body);
                let body_value = translate_expr(&mut self.ctx, *body.value, &vec![]);
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
                        .join(", ");
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
