// -*- mode: Bluespec; -*-

module messaging {
  import cw_types.* from "./cw_types"
  import bank from "./bank"
  import basicSpells.* from "./basicSpells"

  pure def get_message(result_val: Result[Response, ContractError]): (Result[Response, ContractError], Option[SubMsg]) = {
    match result_val {
      | Ok(response) =>
        if (response.messages.indices().size() > 0) {
          val submsg = response.messages.head()
          val new_result = Ok({ ...response, messages: response.messages.listFilter(m => m != submsg) })
          (new_result, Some(submsg))
        } else {
          (result_val, None)
        }

      | _ => (result_val, None)
    }
  }

  pure def process_message(state, env, contract_address, submsg, reply) = {
    match submsg.msg {
      | CosmosMsg_Wasm(wasm_msg) => state // FIXME
      | CosmosMsg_Bank(bank_msg) =>
        match bank_msg {
          | BankMsg_Send(msg) => match bank::send(state.bank, contract_address, msg) {
            | Ok(new_bank) =>
               if (should_reply_on_success(submsg.reply_on)) {
                 val reply_result = reply(state.contract_state, env, { id: submsg.id, result: Ok({ events: [/* TODO */], data: None }) })
                 { bank: new_bank, result: reply_result._1, contract_state: reply_result._2 }
               } else {
                 { ...state, bank: new_bank }
               }
            | Err(err) =>
               if (should_reply_on_error(submsg.reply_on)) {
                 val reply_result = reply(state.contract_state, env, { id: submsg.id, result: Err(err) })
                 { ...state, result: reply_result._1, contract_state: reply_result._2 }
               } else {
                  { ...state, result: Err(err) }
               }
           }
        }
    }
  }
}
