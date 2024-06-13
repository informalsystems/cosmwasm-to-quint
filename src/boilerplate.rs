use itertools::Itertools;

use crate::types::Context;

pub const IMPORTS: &str = "
  import basicSpells.* from \"./lib/basicSpells\"
  import cw_types.* from \"./lib/cw_types\"
  import cw_utils.* from \"./lib/cw_utils\"
  import messaging.* from \"./lib/messaging\"
  import bank from \"./lib/bank\"
";

pub const VARS: &str = "
  var contract_state: ContractState
  var result: Result
  var bank: bank::Bank
  var time: int
";

pub const CONTRACT_ADDRESS: &str = "
  pure val CONTRACT_ADDRESS = \"contract0\"
";

pub const VALUES: &str = "
  pure val ADDRESSES = Set(\"sender1\", \"sender2\", \"sender3\", CONTRACT_ADDRESS)
  pure val DENOMS = Set(\"d1\", \"uawesome\")
  pure val MAX_AMOUNT = 200
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
      match r._1 {
        | Ok(_) => bank' = bank.setBy(sender, balances => balances.setBy(denom, balance => balance - amount))
                               .setBy(CONTRACT_ADDRESS, balances => balances.setBy(denom, balance => balance + amount))
        | Err(_) => bank' = bank
      },
      result' = r._1,
      contract_state' = r._2,
    }
  }

  action advance_time = time' = time + 1

  action step = {
    val message_getting = get_message(result)
    val new_result = message_getting._1
    val opt_message = message_getting._2
    match opt_message {
      | Some(submsg) => {
          val current_state = { bank: bank, result: new_result, contract_state: contract_state }
          val new_state = process_message(current_state, env_val, CONTRACT_ADDRESS, submsg, reply)
          all {
            bank' = new_state.bank,
            result' = new_state.result,
            contract_state' = new_state.contract_state,
            advance_time,
          }
      }
      | None => execute_step
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

pub fn initializers(ctx: &Context) -> String {
    let instatiate_msg = ctx.message_type_for_action.get("instantiate").unwrap();
    format!(
        "
  pure val init_bank_state = ADDRESSES.mapBy(_ => DENOMS.mapBy(_ => MAX_AMOUNT))

  val env_val = {{ block: {{ time: time, height: 1 }} }} // TODO: Add a height var if you need it

  action init = {{
    // TODO: Change next line according to fund expectations
    pure val max_funds = 0

    nondet sender = Set(\"admin\").oneOf()
    nondet denom = DENOMS.oneOf()
    nondet amount = 0.to(max_funds).oneOf()
    val funds = [{{ denom: denom, amount: amount }}]
    val info = {{ sender: sender, funds: funds }}

    pure val message: InstantiateMsg = {}
    pure val r = instantiate(init_contract_state, {{ block: {{ time: 0, height: 1 }} }}, info, message)

    all {{
      contract_state' = r._2,
      bank' = init_bank_state,
      result' = r._1,
      time' = 0,
    }}
  }}
",
        init_value_for_type(ctx, instatiate_msg.clone())
    )
}

// TODO: This doesn't belong here, but I'm not sure where to put it
/// Generates a default value for a given type, used to initialize values of all
/// contract state fields
pub fn init_value_for_type(ctx: &Context, ty: String) -> String {
    if ty.contains("->") {
        return "Map()".to_string();
    }

    if ty.starts_with("List") {
        return "[]".to_string();
    }

    let typ = ty.split('[').next().unwrap();
    if ctx.structs.contains_key(typ) {
        // Type is a struct, initialize fields recursively
        let fields = ctx.structs.get(typ).unwrap();
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
            .join(", ");
        return format!("{{ {} }}", struct_value);
    }

    match typ {
        "str" => "\"\"".to_string(),
        "int" => "0".to_string(),
        "Addr" => "\"s1\"".to_string(),
        _ => {
            eprintln!("No init value for type: {ty}");
            "\"<missing-value>\"".to_string()
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
        .join(",\n    ");

    // After all items were visited, we can produce the complete contract state initializer
    let initializer = ctx
        .contract_state
        .iter()
        .map(|field| format!("{}: {}", field.0, init_value_for_type(ctx, field.1.clone())))
        .collect_vec()
        .join(",\n    ");

    let special_actions = ["execute", "instantiate", "reply"];
    let reply = if !ctx.ops_with_mutability.contains(&"reply".to_string()) {
        // Generate default reply to be given for the message handler
        "
  pure def reply(state: ContractState, _env: Env, _reply: Reply): (Result, ContractState) = (Ok(Response_new), state)
"
    } else {
        "\n"
    };

    let actions = ctx
        .ops_with_mutability
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
{}
{ACTIONS}
}}",
        initializers(ctx)
    )
}
