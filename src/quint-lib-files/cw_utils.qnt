// -*- mode: Bluespec; -*-

// https://docs.rs/cw-utils/0.13.4/src/cw_utils/payment.rs.html#32-39
module cw_utils {
  import cw_types.* from "./cw_types"

  type Expiration =
    // AtHeight will expire when `env.block.height` >= height
    | Expiration_AtHeight(int)
    // AtTime will expire when `env.block.time` >= time
    | Expiration_AtTime(int)
    // Never will never expire. Used to express the empty variant
    | Expiration_Never

  type Duration =
    | Duration_Height(int)
    // Time in seconds
    | Duration_Time(int)

  def one_coin(info: MessageInfo): Result[Coin, ContractError] = {
    if (info.funds.indices().size() == 0) {
      Err("No funds")
    } else if (info.funds.indices().size() == 1) {
      val coin = info.funds[0]
      if (coin.amount == 0) {
        Err("Zero funds")
      } else {
        Ok(coin)
      }
    } else {
      Err("Multiple Denoms")
    }
  }

  def must_pay(info: MessageInfo, denom: str): Result[int, ContractError] = {
    match one_coin(info) {
      | Err(err) => Err(err)
      | Ok(coin) => {
        if (coin.denom != denom) {
          Err("Wrong Denom")
        } else {
          Ok(coin.amount)
        }
      }
    }
  }
}
