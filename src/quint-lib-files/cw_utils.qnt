// -*- mode: Bluespec; -*-

// https://docs.rs/cw-utils/0.13.4/src/cw_utils/payment.rs.html#32-39
module cw_utils {
  import cw_types.* from "./cw_types"

  type OneCoinResult =
    | OneCoin_Ok(Coin)
    | OneCoin_Err(str)

  type MustPayResult =
    | MustPay_Ok(int)
    | MustPay_Err(str)

  def one_coin(info: MessageInfo): OneCoinResult = {
    if (info.funds.indices().size() == 0) {
      OneCoin_Err("No funds")
    } else if (info.funds.indices().size() == 1) {
      val coin = info.funds[0]
      if (coin.amount == 0) {
        OneCoin_Err("No funds")
      } else {
        OneCoin_Ok(coin)
      }
    } else {
      OneCoin_Err("Multiple Denoms")
    }
  }

  def must_pay(info: MessageInfo, denom: str): MustPayResult = {
    match one_coin(info) {
      | OneCoin_Err(err) => MustPay_Err(err)
      | OneCoin_Ok(coin) => {
        if (coin.denom != denom) {
          MustPay_Err("Missing Denom")
        } else {
          MustPay_Ok(coin.amount)
        }
      }
    }
  }
}
