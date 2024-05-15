use crate::types::Context;
use itertools::Itertools;

pub fn translate_structs(ctx: Context) -> String {
    let mut structs = "
pub mod state_structs {
    use num_bigint::BigInt;
    use serde::Deserialize;
    use std::collections::HashMap;
"
    .to_string();
    for (name, fields) in ctx.structs {
        structs.push_str(
            format_struct(
                name,
                fields
                    .iter()
                    .map(|f| (f.name.clone(), f.ty.clone()))
                    .collect_vec(),
                false,
            )
            .as_str(),
        );
    }
    structs.push_str(
        format_struct(
            "ContractState".to_string(),
            ctx.contract_state.clone(),
            false,
        )
        .as_str(),
    );

    structs.push_str(
        format_struct("NondetPicks".to_string(), ctx.nondet_picks.clone(), true).as_str(),
    );

    structs.push_str(BOILERPLATE_STRUCTS);

    format!("{}\n}}", structs)
}

fn format_struct(name: String, fields: Vec<(String, String)>, optional: bool) -> String {
    let fields = fields
        .iter()
        .map(|(name, ty)| {
            let actual_type = translate_type(ty.clone());
            let typ = if optional {
                format!("QuintOption<{}>", actual_type)
            } else {
                actual_type
            };
            format!("        pub {}: {}", name, typ)
        })
        .join(",\n");
    format!(
        "    #[derive(Clone, Debug, Deserialize)]\n  pub struct {} {{\n{}\n    }}\n\n",
        name, fields
    )
}

fn translate_type(ty: String) -> String {
    if ty.contains("->") {
        let it = ty.split("->").collect_vec();
        let key = it[0];
        let value = it[1..].join("->");
        return format!(
            "HashMap<{}, {}>",
            translate_type(key.trim().to_string()),
            translate_type(value.trim().to_string())
        );
    }

    if ty.starts_with("List") {
        return format!(
            "Vec<{}>",
            translate_type(
                ty.trim_start_matches("List[")
                    .trim_end_matches(']')
                    .to_string()
            )
        );
    }

    match ty.as_str() {
        "str" => "String".to_string(),
        "int" => "BigInt".to_string(),
        "Addr" => "String".to_string(),
        _ => ty,
    }
}

const BOILERPLATE_STRUCTS: &str = "
    #[derive(Copy, Clone, Debug, Deserialize)]
    #[serde(tag = \"tag\", content = \"value\")]
    pub enum QuintOption<T> {
        Some(T),
        None,
    }

    #[derive(Copy, Clone, Debug, Deserialize)]
    #[serde(tag = \"tag\", content = \"value\")]
    pub enum QuintResult<T, E> {
        Ok(T),
        Err(E),
    }

    #[derive(Clone, Debug, Deserialize)]
    pub struct Message {}

    #[derive(Clone, Debug, Deserialize)]
    pub struct Response {
        pub messages: Vec<Message>,
    }

    #[derive(Clone, Debug, Deserialize)]
    pub struct State {
        pub contract_state: ContractState,
        pub bank: HashMap<String, HashMap<String, BigInt>>,
        pub result: QuintResult<Response, String>,
        pub action_taken: String,
        pub nondet_picks: NondetPicks,
        pub time: BigInt,
    }

    pub fn to_option<T>(op: QuintOption<T>) -> Option<T> {
        match op {
            QuintOption::None => None,
            QuintOption::Some(a) => Some(a),
        }
    }

    pub fn to_result<T, E>(res: QuintResult<T, E>) -> Result<T, E> {
        match res {
            QuintResult::Ok(a) => Ok(a),
            QuintResult::Err(e) => Err(e),
        }
    }
";
