// -*- mode: Bluespec; -*-

module messaging {
  import cw_types.* from "./cw_types"
  import bank from "./bank"
  import basicSpells.* from "./basicSpells"

  pure def get_message(return_val: Result[Response, ContractError]): (Result[Response, ContractError], Option[SubMsg]) = {
    match return_val {
      | Ok(response) =>
        if (response.messages.indices().size() > 0) {
          val submsg = response.messages.head()
          val new_return = Ok({ ...response, messages: response.messages.listFilter(m => m != submsg) })
          (new_return, Some(submsg))
        } else {
          (return_val, None)
        }

      | _ => (return_val, None)
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
                 { bank: new_bank, return: reply_result._1, contract_state: reply_result._2 }
               } else {
                 { ...state, bank: new_bank }
               }
            | Err(err) =>
               if (should_reply_on_error(submsg.reply_on)) {
                 val reply_result = reply(state.contract_state, env, { id: submsg.id, result: Err(err) })
                 { ...state, return: reply_result._1, contract_state: reply_result._2 }
               } else {
                  state
               }
           }
        }
    }
  }
}
