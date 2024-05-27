use crate::test_generation::boilerplate::*;
use crate::types::Context;
use itertools::Itertools;

/// Defines a module with all necesary structs for the contract state and messages.
pub fn translate_structs(ctx: Context) -> String {
    let contract_structs = ctx
        .structs
        .iter()
        // Sort items by name so that the generated code is deterministic
        .sorted_by(|a, b| a.0.cmp(b.0))
        .map(|(name, fields)| {
            let field_tuples = fields
                .iter()
                .map(|f| (f.name.clone(), f.ty.clone()))
                .collect_vec();

            format_struct(name.to_string(), field_tuples, false)
        })
        .collect_vec()
        .join("\n");

    let contract_state_struct = format_struct(
        "ContractState".to_string(),
        ctx.contract_state.clone(),
        false,
    );

    let nondet_picks_struct =
        format_struct("NondetPicks".to_string(), ctx.nondet_picks.clone(), true);

    format!(
        "
pub mod state_structs {{
    {STRUCTS_MODULE_IMPORTS}
    {contract_structs}
    {contract_state_struct}
    {nondet_picks_struct}
    {DEFAULT_STRUCTS}
}}
    "
    )
}

fn format_struct(name: String, fields: Vec<(String, String)>, optional: bool) -> String {
    let fields = fields
        .iter()
        .map(|(name, ty)| {
            let typ = translate_type(ty.clone());
            if optional {
                format!(
                    "
        #[serde(with = \"As::<de::Option::<_>>\")]
        pub {name}: Option<{typ}>"
                )
            } else {
                format!("pub {name}: {typ}")
            }
        })
        .join(",\n        ");

    format!(
        "
    #[derive(Clone, Debug, Deserialize)]
    pub struct {name} {{
        {fields}
    }}"
    )
}

fn translate_type(ty: String) -> String {
    // FIXME: we should have an intermediate representation for types so we
    // don't have to convert to and from strings like this.
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
