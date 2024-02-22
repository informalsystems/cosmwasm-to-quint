use std::collections::HashMap;

use rustc_middle::ty::TyCtxt;

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

#[derive(Clone)]
pub struct Context<'tcx> {
    pub message_type_for_action: HashMap<String, String>,
    pub constructors: HashMap<String, Constructor>,
    pub stateful_ops: Vec<String>,
    pub structs: HashMap<String, Vec<Field>>,
    pub record_fields: Vec<String>,
    pub current_item_name: String,
    pub tcx: TyCtxt<'tcx>,
    pub contract_state: Vec<(String, String)>,
}
