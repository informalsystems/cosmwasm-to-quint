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
  pure val deps_val = {
    storage: \"TODO\",
    api: \"TODO\",
    querier: \"TODO\"
  }

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

    val r = execute(contract_state, deps_val, env_val, info, message)
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
