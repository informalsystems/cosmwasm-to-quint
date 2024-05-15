use crate::boilerplate::init_value_for_type;
use crate::types::Context;
use itertools::Itertools;

pub fn translate_actions(ctx: Context) -> String {
    let msgs = ctx.message_type_for_action.iter().map(|(action, ty)| {
        if action == "instantiate" {
            let msg_struct = ctx.structs.get("InstantiateMsg").unwrap();
            let msg_fields = msg_struct
                .iter()
                .map(|f| {
                    let body = init_value_for_type(&ctx, f.ty.clone());

                    format!("{}: {}", f.name, body)
                })
                .collect_vec();
            let msg = format!("InstantiateMsg {{ {} }}", msg_fields.join(", "));
            return translate_init(msg);
        }
        if action == "execute" || action == "instantiate" || action == "reply" {
            return "".to_string();
        }
        let constructor = ctx.constructors.get(ty.as_str()).unwrap();
        let nondet_picks = constructor
            .fields
            .iter()
            .map(|f| {
                let body = type_conversion(
                    format!(
                        "to_option(nondet_picks.message_{}.clone()).unwrap()",
                        f.name
                    ),
                    f.ty.clone(),
                );

                format!("                   let message_{} = {};", f.name, body)
            })
            .collect_vec();

        let fields = constructor
            .fields
            .iter()
            .map(|f| format!("{}: message_{}", f.name, f.name))
            .collect_vec()
            .join(", ");
        let msg = format!("{} {{ {} }}", constructor.name.replace('_', "::"), fields);

        translate_action(action, msg, nondet_picks.clone())
    });

    msgs.clone().join("\n")
}

fn translate_action(action: &str, msg: String, nondet_picks: Vec<String>) -> String {
    let header = format!(
        "
               \"{}_action\" => {{
                    let sender = Addr::unchecked(sender.unwrap());
                    let funds = funds_from_trace(amount, denom);

",
        action
    );

    let footer = "
                    println!(\"Message: {:?}\", msg);
                    println!(\"Sender: {:?}\", sender);
                    println!(\"Funds: {:?}\", funds);

                    let res = app.execute_contract(
                        sender,
                        test_state.contract_addr.clone(),
                        &msg,
                        &funds,
                    );

                    compare_result(to_result(s.value.result.clone()), res)
                }

";

    format!(
        "{}{}\n                    let msg = {};{}",
        header,
        nondet_picks.clone().join("\n"),
        msg,
        footer,
    )
}

fn translate_init(msg: String) -> String {
    let header = "
               \"q::init\" => {
                    println!(\"Initializing contract.\");

                    let sender = Addr::unchecked(sender.unwrap());
                    let funds = funds_from_trace(amount, denom);

"
    .to_string();

    let footer = "
                    println!(\"Message: {:?}\", msg);
                    println!(\"Sender: {:?}\", sender);
                    println!(\"Funds: {:?}\", funds);

                    test_state.contract_addr = app.instantiate_contract(
                        code_id,
                        sender,
                        &msg,
                        &funds,
                        \"test\",
                        None,
                    ).unwrap();

                    for (addr, coins) in s.value.bank.clone().iter() {
                        for (denom, amount) in coins.iter() {
                            app = mint_tokens(
                                app,
                                addr.clone(),
                                denom.to_string(),
                                Uint128::new(amount.to_u128().unwrap()),
                            );
                        }
                    }
                }

";

    format!(
        "{}\n                    let msg = {};{}",
        header, msg, footer,
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
        "int" => format!("{}.to_u64().unwrap()", value),
        "Addr" => format!("Addr::unchecked_from({})", value),
        _ => value,
    }
}
