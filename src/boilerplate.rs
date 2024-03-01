use itertools::Itertools;

use crate::types::Context;

pub const IMPORTS: &str = "
  import cw_types.* from \"../lib/cw_types\"
  import messaging.* from \"../lib/messaging\"
  import bank from \"../lib/bank\"
";

pub const VARS: &str = "
  var contract_state: ContractState
  var return: Result
  var bank: bank::Bank
  var time: int
";

pub const CONTRACT_ADDRESS: &str = "
  pure val CONTRACT_ADDRESS = \"<contract>\"
";

pub const VALUES: &str = "
  pure val ADDRESSES = Set(\"s1\", \"s2\", \"s3\", CONTRACT_ADDRESS)
  pure val DENOMS = Set(\"d1\", \"uawesome\")
  pure val MAX_AMOUNT = 200
";

pub const INITIALIZERS: &str = "
  pure val init_bank_state = ADDRESSES.mapBy(_ => DENOMS.mapBy(_ => MAX_AMOUNT))

  val env_val = { block: { time: time } }

  action init = all {
    contract_state' = init_contract_state,
    bank' = init_bank_state,
    return' = Response_Err(\"No previous request\"),
    time' = 0,
  }
";

pub const ACTIONS: &str = "
  action execute_message(message, max_funds) = {
    nondet sender = ADDRESSES.oneOf()
    nondet denom = DENOMS.oneOf()
    nondet amount = 0.to(max_funds).oneOf()
    val funds = [{ denom: denom, amount: amount }]
    val info = { sender: sender, funds: funds }

    val r = execute(contract_state, env_val, info, message)
    all {
      bank.get(sender).get(denom) >= amount,
      bank' = bank.setBy(sender, balances => balances.setBy(denom, balance => balance - amount))
                  .setBy(CONTRACT_ADDRESS, balances => balances.setBy(denom, balance => balance + amount)),
      return' = r._1,
      contract_state' = r._2,
    }
  }

  action advance_time = time' = time + 1

  action step = {
    val message_getting = get_message(return)
    val new_return = message_getting._1
    val opt_message = message_getting._2
    match opt_message {
      | SomeMessage(submsg) => {
          val current_state = { bank: bank, return: new_return, contract_state: contract_state }
          val new_state = process_message(current_state, env_val, CONTRACT_ADDRESS, submsg, reply)
          all {
            bank' = new_state.bank,
            return' = new_state.return,
            contract_state' = new_state.contract_state,
            advance_time,
          }
      }
      | NoneMessage => execute_step
    }
  }
";

pub fn pre_items(crate_name: &str) -> String {
    format!(
        "module {crate_name} {{
  {IMPORTS}
  {VARS}
  {CONTRACT_ADDRESS}
  {VALUES}
"
    )
}

// TODO: This doesn't belong here, but I'm not sure where to put it
/// Generates a default value for a given type, used to initialize values of all
/// contract state fields
fn init_value_for_type(ctx: &Context, ty: String) -> String {
    if ty.contains("->") {
        return "Map()".to_string();
    }

    if ctx.structs.contains_key(&ty) {
        // Type is a struct, initialize fields recursively
        let fields = ctx.structs.get(&ty).unwrap();
        let struct_value = fields
            .iter()
            .map(|field| {
                format!(
                    "{}: {}",
                    field.name,
                    init_value_for_type(ctx, field.ty.clone())
                )
            })
            .collect_vec()
            .join(",");
        return format!("{{ {} }}", struct_value);
    }

    match ty.as_str() {
        "List" => "List()".to_string(),
        "str" => "\"\"".to_string(),
        "int" => "0".to_string(),
        "Addr" => "\"s1\"".to_string(),
        _ => {
            eprintln!("No init value for type: {ty}");
            "<missing-type>".to_string()
        }
    }
}

pub fn post_items(ctx: &Context) -> String {
    // After all items were visited, we can produce the complete contract state type
    let contract_state = ctx
        .contract_state
        .iter()
        .map(|x| format!("{}: {}", x.0, x.1))
        .collect_vec()
        .join(",\n  ");

    // After all items were visited, we can produce the complete contract state initializer
    let initializer = ctx
        .contract_state
        .iter()
        .map(|field| {
            format!(
                "    {}: {}",
                field.0,
                init_value_for_type(ctx, field.1.clone())
            )
        })
        .collect_vec()
        .join(",\n");

    let special_actions = ["execute", "instantiate", "reply"];
    let reply = if !ctx.stateful_ops.contains(&"reply".to_string()) {
        // Generate default reply to be given for the message handler
        "

  pure def reply(state: ContractState, _env: Env, _reply: Reply): (Result, ContractState) = (Response_Ok(Response_new), state)

"
    } else {
        "\n"
    };

    let actions = ctx
        .stateful_ops
        .iter()
        .filter(|op| !special_actions.contains(&op.as_str()))
        .map(|op| format!("{op}_action"))
        .collect_vec()
        .join(",\n      ");

    format!(
        "
  type ContractState = {{
    {contract_state}
  }}

  pure val init_contract_state = {{
    {initializer}
  }}

  action execute_step = all {{
    any {{
      {actions}
    }},
    advance_time,
  }}
{reply}
{INITIALIZERS}
{ACTIONS}
}}"
    )
}
