use crate::boilerplate::init_value_for_type;
use crate::test_generation::boilerplate::*;
use crate::types::{fallback_constructor, Context};
use itertools::Itertools;

pub fn translate_actions(ctx: Context) -> String {
    ctx.message_type_for_action
        .iter()
        // Sort items by name so that the generated code is deterministic
        .sorted_by(|a, b| a.0.cmp(b.0))
        .map(|(action, ty)| {
            if action == "instantiate" {
                let msg = generate_instantiate_msg(ctx.clone());
                return translate_init(msg);
            }

            let msg = generate_message(ctx.clone(), ty.clone());
            translate_action(action, msg)
        })
        .join("\n")
}

fn translate_action(action_name: &str, msg: String) -> String {
    format!(
        "
               \"{action_name}_action\" => {{
                    {EXTRACT_SENDER_AND_FUNDS}
{msg}
                    {PRINT_MESSAGE_FIELDS}
                    {EXECUTE_CONTRACT}
                    {COMPARE_RESULT}
                }}
"
    )
}

fn translate_init(msg: String) -> String {
    format!(
        "
               \"q::init\" => {{
                    println!(\"Initializing contract.\");

                    {EXTRACT_SENDER_AND_FUNDS}
                    {msg}
                    {PRINT_MESSAGE_FIELDS}
                    {INITIALIZE_CONTRACT}
                    {MINT_TOKENS}
               }}

"
    )
}

fn generate_instantiate_msg(ctx: Context) -> String {
    let msg_struct = ctx
        .structs
        .get("InstantiateMsg")
        .cloned()
        .unwrap_or_default();

    let msg_fields = msg_struct
        .iter()
        .map(|f| {
            let body = init_value_for_type(&ctx, f.ty.clone());

            format!("{}: {}", f.name, body)
        })
        .collect_vec();

    format!("let msg = InstantiateMsg {{ {} }};", msg_fields.join(", "))
}

fn generate_message(ctx: Context, ty: String) -> String {
    let constructor = ctx
        .constructors
        .get(ty.as_str())
        .cloned()
        .unwrap_or_else(|| fallback_constructor(ty.as_ref()));

    let nondet_picks = constructor
        .fields
        .iter()
        .map(|f| {
            let body = type_conversion(
                format!("nondet_picks.message_{}.clone().unwrap()", f.name),
                f.ty.clone(),
            );

            format!("                   let message_{} = {};", f.name, body)
        })
        .collect_vec()
        .join("\n");

    let fields = constructor
        .fields
        .iter()
        .map(|f| format!("{}: message_{}", f.name, f.name))
        .collect_vec()
        .join(", ");

    let message_type = constructor.name.replace('_', "::");

    format!(
        "{nondet_picks}
                    let msg = {message_type} {{ {fields} }};"
    )
}

fn type_conversion(value: String, ty: String) -> String {
    if ty.starts_with("List") {
        return format!(
            "{}.iter().map(|x| {}).collect()",
            value,
            type_conversion(
                "x".to_string(),
                ty.trim_start_matches("List[")
                    .trim_end_matches(']')
                    .to_string()
            )
        );
    }

    match ty.as_str() {
        "str" => value,
        "int" => format!("{}.to_u64().unwrap().into()", value),
        "Addr" => format!("Addr::unchecked_from({})", value),
        _ => value,
    }
}
