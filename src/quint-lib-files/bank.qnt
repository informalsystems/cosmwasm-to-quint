// -*- mode: Bluespec; -*-

module bank {
  import cw_types.* from "./cw_types"
  type Bank = Addr -> Denom -> int

  pure def send(bank: Bank, source_addr: Addr, msg: { to_address: Addr, amount: List[Coin] }): Result[Bank, ContractError] = {
    // FIXME: consider multiple coins
    pure val amount = msg.amount[0].amount
    pure val denom = msg.amount[0].denom
    if (amount >= 0 and bank.get(source_addr).get(denom) >= amount) {
      val new_bank = bank
        .setBy(source_addr, balances => balances.setBy(denom, balance => balance - amount))
        .setBy(msg.to_address, balances => balances.setBy(denom, balance => balance + amount))

      Ok(new_bank)
    } else {
      Err("Not enough balance")
    }
  }

  pure def balance(bank: Bank, address: Addr, denom: Denom): Coin = {
    { amount: bank.get(address).get(denom), denom: denom }
  }
}
