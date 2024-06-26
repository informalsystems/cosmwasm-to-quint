use std::collections::HashMap;

use rustc_middle::ty::TyCtxt;

use crate::nondet::NondetInfo;

#[derive(Clone, Debug)]
pub struct Field {
    pub name: String,
    pub ty: String,
    pub nondet_info: NondetInfo,
}

#[derive(Clone, Debug)]
pub struct Constructor {
    pub name: String,
    pub result_type: String,
    pub fields: Vec<Field>,
}

// It's impossible to translate the declaration and the body separately because
// of param names and types. Therefore, this intermediate representation is
// necessary.
#[derive(Clone, Debug)]
pub struct Function<'f> {
    pub decl: rustc_hir::FnDecl<'f>,
    pub body: rustc_hir::Body<'f>,
}

#[derive(Clone)]
pub struct Context<'tcx, 'c> {
    // global
    pub crate_name: &'c str,
    pub message_type_for_action: HashMap<String, String>,
    pub constructors: HashMap<String, Constructor>,
    pub structs: HashMap<String, Vec<Field>>,
    pub ops_with_mutability: Vec<String>,
    pub tcx: TyCtxt<'tcx>,
    pub contract_state: Vec<(String, String)>,
    pub nondet_picks: Vec<(String, String)>,
    pub enums: HashMap<String, Vec<String>>,
    pub type_aliases: HashMap<String, String>,
    // scoped
    // FIXME: This should be a stack to account for nested scopes.
    // No need to worry about nested scopes for stub generation.
    pub record_fields: Vec<String>,
    pub struct_fields: Vec<Field>,
    pub pat_fields: Vec<String>,
    pub generics: Vec<String>,
    pub current_item_name: String,
}

pub fn fallback_constructor(name: &str) -> Constructor {
    eprintln!("No message type for action: {name}");
    Constructor {
        name: format!("ConstructorFor{}", name),
        result_type: "UnknownType".to_string(),
        fields: vec![],
    }
}
