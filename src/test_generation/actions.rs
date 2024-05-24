use crate::boilerplate::init_value_for_type;
use crate::test_generation::boilerplate::*;
use crate::types::{fallback_constructor, Context};
use itertools::Itertools;

/// Generates one match arm per action on the model:
///   "foo_action" => // run foo and compare result
///   "bar_action" => // run bar and compare result
pub fn arms_for_actions(ctx: Context) -> String {
    ctx.message_type_for_action
        .iter()
        // Sort items by name so that the generated code is deterministic
        .sorted_by(|a, b| a.0.cmp(b.0))
        .map(|(action, ty)| {
            if action == "instantiate" {
                let msg = generate_instantiate_msg(ctx.clone());
                return arm_for_init(msg);
            }

            let msg = generate_message(ctx.clone(), ty.clone());
            arm_for_action(action, msg)
        })
        .join("\n")
}

fn arm_for_action(action_name: &str, msg: String) -> String {
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

fn arm_for_init(msg: String) -> String {
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

            format!("let message_{} = {};", f.name, body)
        })
        .collect_vec();

    let fields = constructor
        .fields
        .iter()
        .map(|f| format!("{}: message_{}", f.name, f.name))
        .collect_vec()
        .join(", ");

    let message_type = constructor.name.replace('_', "::");

    let msg_def = format!("let msg = {message_type} {{ {fields} }};");

    [nondet_picks, vec![msg_def]]
        .concat()
        .join("\n                    ")
}

fn type_conversion(value: String, ty: String) -> String {
    if ty.starts_with("List") {
        // FIXME: we should have an intermediate representation for types so we
        // don't have to convert to and from strings like this.
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

    // TODO: convert other types

    match ty.as_str() {
        "str" => value,
        "int" => format!("{}.to_u64().unwrap().into()", value),
        "Addr" => format!("Addr::unchecked_from({})", value),
        _ => value,
    }
}
