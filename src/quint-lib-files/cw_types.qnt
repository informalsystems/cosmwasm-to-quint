// -*- mode: Bluespec; -*-

module cw_types {
  import basicSpells.* from "./basicSpells"

  type Addr = str
  type Denom = str

  type TODO = str
  type ContractError = str

  type ReplyOn =
    | ReplyOn_Always
    | ReplyOn_Error
    | ReplyOn_Success
    | ReplyOn_Never

  type SubMsg = { id: int, msg: CosmosMsg, reply_on: ReplyOn, gas_limit: Option[int] }
  type Attribute = { key: str, value: CW_Serialized }
  type Event = { ty: str, attributes: List[Attribute] }

  type Response = {
    messages: List[SubMsg],
    attributes: List[Attribute],
    events: List[Event],
    data: Option[str],
  }

  type CosmosMsg =
    | CosmosMsg_Wasm(WasmMsg)
    | CosmosMsg_Bank(BankMsg)
    // | CosmosMsg_Custom(T)
    // | CosmosMsg_Staking(StakingMsg)
    // | CosmosMsg_Distribution(DistributionMsg)

  type WasmMsg =
    | WasmMsg_Instantiate({
        admin: Option[str],
        code_id: int,
        // msg is the JSON-encoded InstantiateMsg struct (as raw Binary)
        // msg: InstantiateMsg, // TODO: we need polymorphism
        funds: List[Coin],
        // A human-readable label for the contract.
        //
        // Valid values should:
        // - not be empty
        // - not be bigger than 128 bytes (or some chain-specific limit)
        // - not start / end with whitespace
        label: str,
    })
    // | Execute(...)

  type BankMsg =
    | BankMsg_Send({ to_address: str, amount: List[Coin] })

  pure val Response_new = { messages: [], attributes: [], events: [], data: None }

  /// This is used for cases when we use ReplyOn::Never and the id doesn't matter
  pure val UNUSED_MSG_ID = 0

  pure def SubMsg_new(msg: CosmosMsg): SubMsg = {
    id: UNUSED_MSG_ID,
    msg: msg,
    reply_on: ReplyOn_Never,
    gas_limit: None,
  }

  pure def SubMsg_reply_on_success(msg: CosmosMsg, id: int): SubMsg = {
    __reply_on(msg, id, ReplyOn_Success)
  }

  pure def SubMsg_reply_on_error(msg: CosmosMsg, id: int): SubMsg = {
    __reply_on(msg, id, ReplyOn_Error)
  }

  pure def SubMsg_reply_always(msg: CosmosMsg, id: int): SubMsg = {
    __reply_on(msg, id, ReplyOn_Always)
  }

  pure def with_gas_limit(msg: SubMsg, limit: int): SubMsg = {
    { ...msg, gas_limit: Some(limit) }
  }

  pure def __reply_on(msg: CosmosMsg, id: int, reply_on: ReplyOn): SubMsg = {
    {
      id: id,
      msg: msg,
      reply_on: reply_on,
      gas_limit: None,
    }
  }

  def should_reply_on_success(r) = match r {
    | ReplyOn_Always => true
    | ReplyOn_Success => true
    | _ => false
  }

  def should_reply_on_error(r: ReplyOn): bool = match r {
    | ReplyOn_Always => true
    | ReplyOn_Error => true
    | _ => false
  }

  type SubMsgResponse = {
    events: List[Event],
    data: Option[CW_Serialized]
  }

  type Reply = {
    id: int,
    result: Result[SubMsgResponse, ContractError],
  }

  pure def add_attribute(r: Response, k: str, v: CW_Serialized): Response = {
    val attribute = { key: k, value: v }
    { ...r, attributes: r.attributes.append(attribute) }
  }

  pure def add_message(r: Response, m: CosmosMsg): Response = {
    { ...r, messages: r.messages.append(SubMsg_new(m)) }
  }

  pure def add_submessage(r: Response, m: SubMsg): Response = {
    { ...r, messages: r.messages.append(m) }
  }

  type Result[ok, err] = Ok(ok) | Err(err)

  type Deps = {
    storage: TODO,
    api: TODO,
    querier: TODO,
  }

  type BlockInfo = { time: int, height: int }
  type Env = { block: BlockInfo }

  type Coin = {
    denom: str,
    amount: int
  }

  type MessageInfo = {
    sender: str,
    funds: List[Coin]
  }

  type CW_Serialized =
    | FromStr(str)
    | FromInt(int)
    | FromListInt(List[int])

  // These shouldn't be actually used, but they are here for completion
  type MultiIndex[ik, t, pk] = ()
  type IndexedMap[k, t i] = ()

  type StdError = str
  type StdResult[a] = Result[a, StdError]

  // There's no dynamic dispatch in Quint, so the best we can do is to have a
  // special string argument to identify a behavior and then have the
  // implementation depend on it
  type TraitObject = str
}
