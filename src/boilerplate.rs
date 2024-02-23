use std::collections::HashMap;

use itertools::Itertools;

use crate::types::Context;

pub const IMPORTS: &str = "
  import cw_types.* from \"../lib/cw_types\"
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
      return' = r._1,
      contract_state' = r._2,
    }
  }

  action advance_time = time' = time + 1

  action step = {
    match return {
      | Response_Ok(response) =>
        if (response.messages.indices().size() > 0) all {
          // Bank stuff
          match response.messages.head() {
            | BankMsg_Send(msg) => all {
                bank' = bank::send(bank, CONTRACT_ADDRESS, msg),
                contract_state' = contract_state,
                return' = Response_Ok({ ...response, messages: response.messages.tail() }),
                advance_time,
             }
          }
        } else {
          execute_step
        }
      | _ => execute_step
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

    // Type is a primitive, return a default value
    let init_values_by_type: HashMap<&str, &str> = HashMap::from([
        ("List", "List()"),
        ("str", "\"\""),
        ("int", "0"),
        ("Addr", "\"s1\""),
    ]);

    init_values_by_type
        .get(ty.as_str())
        .unwrap_or_else(|| {
            eprintln!("No init value for type: {ty}");
            &"<missing-type>"
        })
        .to_string()
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
                init_value_for_type(&ctx, field.1.clone())
            )
        })
        .collect_vec()
        .join(",\n");

    let actions = ctx
        .stateful_ops
        .iter()
        .filter(|op| *op != &"execute".to_string() && *op != &"instantiate".to_string())
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
    bank' = bank,
  }}

{INITIALIZERS}
{ACTIONS}
}}"
    )
}
