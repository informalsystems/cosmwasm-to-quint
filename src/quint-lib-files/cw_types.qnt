// -*- mode: Bluespec; -*-

module cw_types {
  type Addr = str
  type Denom = str

  type TODO = str
  type ContractError = str

  type ReplyOn =
    | ReplyOn_Always
    | ReplyOn_Error
    | ReplyOn_Success
    | ReplyOn_Never

  type SubMsg = { id: int, msg: CosmosMsg, reply_on: ReplyOn, gas_limit: OptionalInt }
  type Attribute = { key: str, value: CW_Serialized }
  type Event = { ty: str, attributes: List[Attribute] }

  type Response = {
    messages: List[SubMsg],
    attributes: List[Attribute],
    events: List[Event],
    data: OptionalString,
  }

  type CosmosMsg =
    | CosmosMsg_Wasm(WasmMsg)
    | CosmosMsg_Bank(BankMsg)
    // | CosmosMsg_Custom(T)
    // | CosmosMsg_Staking(StakingMsg)
    // | CosmosMsg_Distribution(DistributionMsg)

  type WasmMsg =
    | WasmMsg_Instantiate({
        admin: OptionalString,
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

  pure val Response_new = { messages: [], attributes: [], events: [], data: NoneString }

  /// This is used for cases when we use ReplyOn::Never and the id doesn't matter
  pure val UNUSED_MSG_ID = 0

  pure def SubMsg_new(msg: CosmosMsg): SubMsg = {
    id: UNUSED_MSG_ID,
    msg: msg,
    reply_on: ReplyOn_Never,
    gas_limit: NoneInt,
  }

  pure def SubMsg_reply_on_sucess(msg: CosmosMsg, id: int): SubMsg = {
    __reply_on(msg, id, ReplyOn_Success)
  }

  pure def SubMsg_reply_on_error(msg: CosmosMsg, id: int): SubMsg = {
    __reply_on(msg, id, ReplyOn_Error)
  }

  pure def SubMsg_reply_always(msg: CosmosMsg, id: int): SubMsg = {
    __reply_on(msg, id, ReplyOn_Always)
  }

  pure def with_gas_limit(msg: SubMsg, limit: int): SubMsg = {
    { ...msg, gas_limit: SomeInt(limit) }
  }

  pure def __reply_on(msg: CosmosMsg, id: int, reply_on: ReplyOn): SubMsg = {
    {
      id: id,
      msg: msg,
      reply_on: reply_on,
      gas_limit: NoneInt,
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

  type SubMsgResult = SubMsgResult_Ok(SubMsgResponse) | SubMsgResult_Err(str)

  type SubMsgResponse = {
    events: List[Event],
    data: Optional_CW_Serialized
  }

  type Reply = {
    id: int,
    result: SubMsgResult,
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

  type Env = { block: { time: int /*, height: int */ } } // TODO

  type Coin = {
    denom: str,
    amount: int
  }

  type MessageInfo = {
    sender: str,
    funds: List[Coin]
  }

  // TODO: make polymorphic
  type OptionalString = SomeString(str) | NoneString
  type OptionalInt = SomeInt(int) | NoneInt

  type CW_Serialized =
    | FromStr(str)
    | FromInt(int)
    | FromListInt(List[int])

  type CW_SerializedResult = CW_Serialized_Ok(CW_Serialized) | CW_Serialized_Err(str)
  type Optional_CW_Serialized = Some_CW_Serialized(CW_Serialized) | None_CW_Serialized
}

