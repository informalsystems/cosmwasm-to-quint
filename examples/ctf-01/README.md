# Walk-through to capturing the flag on CTF 01

This is a walk-through on how we can find the bug in the CTF 01 contract using
[Quint](https://github.com/informalsystems/quint) with
[cosmwasm-to-quint](https://github.com/informalsystems/cosmwasm-to-quint).

## 1. Setup

First of all, we need to set up our tools. We should, if we haven't already,
install Rust and
[Quint](https://github.com/informalsystems/quint?tab=readme-ov-file#installation),
and clone the [cosmwasm-ctf](https://github.com/oak-security/cosmwasm-ctf/) repo
to access the challenge.

We also need to [install
cosmwasm-to-quint](https://github.com/informalsystems/cosmwasm-to-quint/?tab=readme-ov-file#setup-and-generation)
and follow the instructions to configure our project (CTF 01):

```bash
cd /path/to/cosmwasm-ctf/ctf-01

# Install and use a nightly version
rustup toolchain add nightly-2024-01-06
rustup override set nightly-2024-01-06
# Add components and target
rustup component add clippy rust-src rustc-dev llvm-tools-preview
rustup target add wasm32-unknown-unknown
# Update dependencies on Cargo.lock to match the new tool chain
cargo update
```

We should also add all dev dependencies required to run the generated tests, as instructed in cosmwasm-to-quint readme:
```bash
cargo add cw-multi-test@0.16.2 --dev
cargo add itf@0.2.4 --dev
cargo add anyhow@1.0.83 --dev
cargo add num-bigint@0.4.4 --dev
cargo add num-traits@0.2.17 --dev
```

## 2. Generation

We can now generate the model stubs and tests for the CTF contract by running:
```bash
cargo clean && cargo cosmwasm-to-quint
```

which should print a message like this at the end:
```
Sucessfully generated files. Quint files are inside quint/ and test files are inside tests/.
```

And now we have the generated files in their respective folders:
```bash
$ tree quint tests
quint
├── lib
│   ├── bank.qnt
│   ├── basicSpells.qnt
│   ├── BoundedUInt.qnt
│   ├── cw_types.qnt
│   ├── cw_utils.qnt
│   └── messaging.qnt
└── oaksecurity_cosmwasm_ctf_01_stubs.qnt
tests
└── mbt_oaksecurity_cosmwasm_ctf_01.rs

3 directories, 8 files
```

We can now try to run `cargo test`: this should give us an error like:
```
couldn't read tests/../quint/test.itf.json: No such file or directory
```

That makes sense: we need a trace from the model in order to run our model-based
test.

## 3. Obtaining some trace

To obtain some random trace, we can run quint simulator with a single sample (`--max-samples=1`):
```
quint run quint/oaksecurity_cosmwasm_ctf_01_stubs.qnt --mbt --out-itf=quint/test.itf.json --max-samples=1
```

And then we should be able to run the tests with `cargo test`. However, we get a test failure:
```
Action unexpectedly failed [...]
Caused by:
    Unauthorized
```

That's because our model is still made of stubs and doesn't have any of the
"business logic" of the contract. So the tests that should ensure model and
implementation match are expected to fail - all good!

## 4. Getting the model and the implementation to match

This is the part that takes the most effort: we need to replace the `// TODO Update body` placeholders with the actual definition for the body. Here's a diff for what we need to add for the CTF-01 contract: the instantiate, deposit and withdraw entrypoint bodies.
```diff
30,31c30
<     // TODO: Update body
<     (Ok(Response_new), state)
---
>     (Ok(Response_new.add_attribute("action", FromStr("instantiate"))), state)
40,41c39,62
<     // TODO: Update body
<     (Ok(Response_new), state)
---
>     match must_pay(info, DENOM) {
>       | Err(s) => (Err(s), state)
>       | Ok(amount) => {
>         if(amount < MINIMUM_DEPOSIT_AMOUNT) {
>           (Err("Unauthorized"), state)
>         } else {
>           val id = state.last_id
>           val release_timestamp = env.block.time + LOCK_PERIOD
>           val lock = {
>             id: id,
>             owner: info.sender,
>             amount: amount,
>             release_timestamp: release_timestamp
>           }
>           val new_state = { ...state, lockups: { state.lockups.put(id, lock) }, last_id: id + 1 }
>           (Ok(Response_new.add_attribute("action", FromStr("deposit"))
>                           .add_attribute("id", FromInt(id))
>                           .add_attribute("owner", FromStr(lock.owner))
>                           .add_attribute("amout", FromInt(amount))
>                           .add_attribute("release_timestamp", FromInt(release_timestamp))
>              ), new_state)
>         }
>       }
>     }
53,54c74,98
<     // TODO: Update body
<     (Ok(Response_new), state)
---
>      val lockups = ids.listMap(id => state.lockups.get(id))
>
>     if (lockups.toSet().exists(lockup => lockup.owner != info.sender or env.block.time < lockup.release_timestamp)) {
>       (Err("Unauthorized"), state)
>     } else {
>       val total_amount = lockups.foldl(0, (acc, lockup) => {
>         acc + lockup.amount
>       })
>       val new_state = lockups.foldl(state, (acc, lockup) => {
>         { ...acc, lockups: acc.lockups.mapRemove(lockup.id) }
>       })
>
>       val msg = BankMsg_Send({
>         to_address: info.sender,
>         amount: [{
>           denom: DENOM,
>           amount: total_amount,
>         }]
>       })
>
>       (Ok(Response_new.add_attribute("action", FromStr("withdraw"))
>                       .add_attribute("ids", FromListInt(ids))
>                       .add_attribute("total_amount", FromInt(total_amount))
>                       .add_message(CosmosMsg_Bank(msg))), new_state)
>     }
```

Now, we need to run the simulator again to get a new trace for the updated model:
```bash
quint run quint/oaksecurity_cosmwasm_ctf_01_stubs.qnt --mbt --out-itf=quint/test.itf.json --max-samples=1
```

But there's an issue with our model, and we get the following error:
```bash
error: [QNT507] quint/oaksecurity_cosmwasm_ctf_01_stubs.qnt:74:38 - error: Called 'get' with a non-existing key
74:      val lockups = ids.listMap(id => state.lockups.get(id))
                                         ^^^^^^^^^^^^^^^^^^^^^

error: Runtime error
```

Seems like we are trying to withdraw non-existent lockups. To fix the error, we
need to ensure this never happens. So let's change two things:
1. `withdraw_action` should only be possible if there are any lockups
2. The value for `message_ids` (the field of the withdraw message) should have elements from the set of existing lockups, and it should be a non-empty list.
```diff
101c101,102
<   action withdraw_action = {
---
>   action withdraw_action = all {
>     contract_state.lockups.keys().size() > 0,
104,105c105,106
<     nondet message_ids: List[int] = 0.to(MAX_AMOUNT).allListsUpTo(2).oneOf()
<     pure val message: ExecuteMsg = ExecuteMsg_Withdraw({ ids: message_ids })
---
>     nondet message_ids: List[int] = contract_state.lockups.keys().allListsUpTo(2).filter(l => l.length() > 0).oneOf()
>     val message: ExecuteMsg = ExecuteMsg_Withdraw({ ids: message_ids })
```

This might seem a bit complicated for now, but it should become fairly
straightforward after you learn a bit of Quint.

After this fix, we can successfully generate a new trace
```bash
quint run quint/oaksecurity_cosmwasm_ctf_01_stubs.qnt --mbt --out-itf=quint/test.itf.json --max-samples=1
```

And then we can try running `cargo test` again. But we get another error!
```bash
Step number: Some(1)
Result from trace: Err("Wrong Denom")
Message: Deposit
Sender: Addr("sender1")
Funds: [Coin { 111 "d1" }]
thread 'tests::model_test' panicked at src/contract.rs:42:41:
called `Result::unwrap()` on an `Err` value: MissingDenom("uawesome")
```
PS: your error might look a bit different since has information about the randomly generated trace.

Well, this is not exactly a problem: both the contract and the model (trace) have complained about the denom. In the funds, we can see that the denom was "d1" while the contract expects "uawesome". The problem is that the contract panics in this scenario, and we can't continue testing. So we need to prevent this panic - by preventing the model to generate traces with incorrect funds:
```diff
20c20
<   pure val DENOMS = Set("d1", "uawesome")
---
>   pure val DENOMS = Set("uawesome")
```

And yet again, we generate a trace:
```bash
quint run quint/oaksecurity_cosmwasm_ctf_01_stubs.qnt --mbt --out-itf=quint/test.itf.json --max-samples=1
```

But now we got it! Running `cargo test` now succeeds. This means our model is
currently matching our implementation. At least, for a random trace. We can be
more sure by re-generating the trace a few times and checking that `cargo test`
succeeds for each of them.

## 5. Obtaining a trace for the challenge's goal

Now we can start trying to solve the challenge. The goal for CTF 01 is: >
Demonstrate how an unprivileged user can drain all funds inside the contract.

Let's write an invariant for that. We can actually simplify this a bit by defining that the contract should not be drained, that is, its balance should never be less than what it was at the start.
```bluespec
val contract_balance_ok = bank.get(CONTRACT_ADDRESS).get(DENOM) >= 200 // initial balance
```

This invariant is written in Quint, and we need to add it to our Quint file. We
usually define the invariants at the very end of the module.

Let's try to find a counterexample for this invariant:
```bash
quint run quint/oaksecurity_cosmwasm_ctf_01_stubs.qnt --mbt --out-itf=quint/test.itf.json --invariant=contract_balance_ok
```

That doesn't really work. We should see an "Invariant violated" message on
STDOUT, but we don't see that. This means the invariant was not violated, so we
couldn't find the scenario we wanted. Let's run it again without the `--out-itf`
argument, so we can examine the pretty output.

```
$ quint run quint/oaksecurity_cosmwasm_ctf_01_stubs.qnt --mbt --invariant=contract_balance_ok
An example execution:

[State 0]
{
  action_taken: "q::init",
  bank:
    Map(
      "contract0" -> Map("uawesome" -> 200),
      "sender1" -> Map("uawesome" -> 200),
      "sender2" -> Map("uawesome" -> 200),
      "sender3" -> Map("uawesome" -> 200)
    ),
  contract_state: { last_id: 0, lockups: Map() },
  nondet_picks: { amount: Some(0), denom: Some("uawesome"), message_ids: None, sender: Some("admin") },
  result:
    Ok({
      attributes: [{ key: "action", value: FromStr("instantiate") }],
      data: None,
      events: [],
      messages: []
    }),
  time: 0
}

[State 1]
{
  action_taken: "deposit_action",
  bank:
    Map(
      "contract0" -> Map("uawesome" -> 200),
      "sender1" -> Map("uawesome" -> 200),
      "sender2" -> Map("uawesome" -> 200),
      "sender3" -> Map("uawesome" -> 200)
    ),
  contract_state: { last_id: 0, lockups: Map() },
  nondet_picks: { amount: Some(193), denom: Some("uawesome"), message_ids: None, sender: Some("sender1") },
  result: Err("Unauthorized"),
  time: 1
}

[State 2]
{
  action_taken: "deposit_action",
  bank:
    Map(
      "contract0" -> Map("uawesome" -> 200),
      "sender1" -> Map("uawesome" -> 200),
      "sender2" -> Map("uawesome" -> 200),
      "sender3" -> Map("uawesome" -> 200)
    ),
  contract_state: { last_id: 0, lockups: Map() },
  nondet_picks: { amount: Some(134), denom: Some("uawesome"), message_ids: None, sender: Some("sender3") },
  result: Err("Unauthorized"),
  time: 2
}

[...]
```

The actual trace has 20 states, but is more of the same: by examining the trace
a bit, we can see that `action_taken` is always `deposit_action`, and the
`result` is always `Unauthorized` or some other error. We must be doing
something wrong in the validation of the deposit action, let's take a look at
the part of the model responsible for this `Unauthorized` response:

```bluespec
if (amount < MINIMUM_DEPOSIT_AMOUNT) {
  (Err("Unauthorized"), state)
} else {
  // ...
}
```

From this, we can figure out that there might be something wrong with the amounts. And there is. While the `MINIMUM_DEPOSIT_AMOUNT` of the contract is `10_000`, the model's `MAX_AMOUNT`, used to generate integer inputs, is `200`. That is, there will never be a deposit action with a sufficient amount to go to the `else` branch. In order to fix that, we can either reduce the `MINIMUM_DEPOSIT_AMOUNT` or increase the `MAX_AMOUNT`. Let's do the former:
On the model:
```diff
110c110
<   pure val MINIMUM_DEPOSIT_AMOUNT = 10000
---
>   pure val MINIMUM_DEPOSIT_AMOUNT = 100
```

On the contract:
```diff
diff --git a/ctf-01/src/contract.rs b/ctf-01/src/contract.rs
index ad9b617..4532e24 100644
--- a/ctf-01/src/contract.rs
+++ b/ctf-01/src/contract.rs
@@ -10,7 +10,7 @@ use crate::state::{Lockup, LAST_ID, LOCKUPS};
 use cw_utils::must_pay;

 pub const DENOM: &str = "uawesome";
-pub const MINIMUM_DEPOSIT_AMOUNT: Uint128 = Uint128::new(10_000);
+pub const MINIMUM_DEPOSIT_AMOUNT: Uint128 = Uint128::new(100);
 pub const LOCK_PERIOD: u64 = 60 * 60 * 24;

 #[cfg_attr(not(feature = "library"), entry_point)]
```

Again, no invariant violation. Let's repeat the process (run without `--out-itf`) and inspect the trace. We can see that all of our `withdraw` actions look like this:
```
[State 19]
{
  action_taken: "withdraw_action",
  bank:
    Map(
      "contract0" -> Map("uawesome" -> 543),
      "sender1" -> Map("uawesome" -> 14),
      "sender2" -> Map("uawesome" -> 200),
      "sender3" -> Map("uawesome" -> 43)
    ),
  contract_state:
    {
      last_id: 4,
      lockups:
        Map(
          0 -> { amount: 186, id: 0, owner: "sender1", release_timestamp: 86403 },
          1 -> { amount: 101, id: 1, owner: "contract0", release_timestamp: 86408 },
          2 -> { amount: 157, id: 2, owner: "sender3", release_timestamp: 86413 },
          3 -> { amount: 172, id: 3, owner: "contract0", release_timestamp: 86417 }
        )
    },
  nondet_picks:
    { amount: Some(169), denom: Some("uawesome"), message_ids: Some([3, 1]), sender: Some("contract0") },
  result: Err("Unauthorized"),
  time: 19
}
```

Again, let's try to figure out why we are getting so much `Unauthorized`.
```bluespec
if (lockups.toSet().exists(lockup => lockup.owner != info.sender or env.block.time < lockup.release_timestamp)) {
  (Err("Unauthorized"), state)
} else {
  // ...
}
```

We can investigate for a bit, but we'll find that we are not properly handling timestamps. On the trace, the time is `19`, while our `LOCK_PERIOD` for lockups is `60 * 60 * 24`. By default, the generated model and test tick in intervals of 1 second, so it would take a really long trace to get a lockup to be release. Let's change that by increasing our `TICK` to `LOCK_PERIOD`:
On the model:
```diff
180c180
<   action advance_time = time' = time + 1
---
>   action advance_time = time' = time + LOCK_PERIOD
```

On the test file:
```diff
88c88
<     pub const TICK: u64 = 1;
---
>     pub const TICK: u64 = contract::LOCK_PERIOD;
```

Now let's try again, and we finally got a violated invariant. We can also run without `--out-itf` to see the trace that achieves the goal:
```bash
[State 14]
{
  action_taken: "withdraw_action",
  bank:
    Map(
      "contract0" -> Map("uawesome" -> 170),
      "sender1" -> Map("uawesome" -> 200),
      "sender2" -> Map("uawesome" -> 230),
      "sender3" -> Map("uawesome" -> 200)
    ),
  contract_state: { last_id: 2, lockups: Map() },
  nondet_picks:
    { amount: Some(78), denom: Some("uawesome"), message_ids: Some([0, 0]), sender: Some("sender2") },
  result:
    Ok({
      attributes:
        [
          { key: "action", value: FromStr("withdraw") },
          { key: "ids", value: FromListInt([0, 0]) },
          { key: "total_amount", value: FromInt(216) }
        ],
      data: None,
      events: [],
      messages: []
    }),
  time: 1209600
}
```

Looks like the contract got drained after an withdraw of ids `[0, 0]`.

Now let's replay this trace in the implementation by running `cargo test`. Unexpectedly, we get an error:
```
Step number: Some(2)
Result from trace: Err("Unauthorized")
Message: Withdraw { ids: [0] }
Sender: Addr("sender1")
Funds: [Coin { 15 "uawesome" }]
thread 'tests::model_test' panicked at src/contract.rs:83:60:
called `Result::unwrap()` on an `Err` value: NotFound
```

Seems like the implementation broke when we tried to withdraw id 0. Somehow, the model and the implementation are not matching. This might actually be due to a minor issue in the CTF contract: although the instantiate message includes a `count` field, this is never used. So even we are configuring both the model and the implementation with an initial count of 0, the implementation has a hard-coded 1. Let's update our model to match this:
```diff
119c119
<     last_id: 0,
---
>     last_id: 1,
```

We can now generate a violation trace one more time and run `cargo test` to see the test finally succeeds! This means we were able to reproduce steps leading to the challenge's goal in the actual implementation. If we want to be sure, we can run `cargo test -- --nocapture` to see the printed information. On the last state, we'll see something like this:
```bash
Contract balance (Addr("contract0")) for uawesome: Uint128(114) vs Uint128(114)
```
Which means that our contract has `114` uawesome tokens, which is less than it's start amount of `200`.

Although this doesn't really work at the first attempt, it's usually
straightforward to find the issues and fix them, and we learn a lot about
details and ambiguities while doing it.

