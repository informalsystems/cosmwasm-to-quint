pub const TEST_HEADER: &str = "
#[cfg(test)]
pub mod tests {
    use crate::{
        contract::{DENOM, LOCK_PERIOD},
        mbt::state_structs::*,
        msg::{ExecuteMsg, InstantiateMsg, QueryMsg},
    };
    use cosmwasm_std::{coin, Addr, Uint128};
    use cw_multi_test::{App, AppResponse, ContractWrapper, Executor};
    use itf::trace_from_str;
    use num_bigint::BigInt;
    use num_traits::{ToPrimitive, Zero};

    pub const ADMIN: &str = \"admin\";

    pub fn mint_tokens(mut app: App, recipient: String, denom: String, amount: Uint128) -> App {
        app.sudo(cw_multi_test::SudoMsg::Bank(
            cw_multi_test::BankSudo::Mint {
                to_address: recipient.to_owned(),
                amount: vec![coin(amount.u128(), denom)],
            },
        ))
        .unwrap();
        app
    }

    fn compare_state(test_state: &TestState, app: &App, state: &State) {
        // compare contract balances
        let balance = app
            .wrap()
            .query_balance(&test_state.contract_addr, DENOM)
            .unwrap()
            .amount;
        let trace_balance = state
            .bank
            .get(&test_state.contract_addr.to_string())
            .and_then(|x| x.get(DENOM))
            .and_then(|x| x.to_u128())
            .unwrap_or(0);
        println!(
            \"Contract balance ({:?}) for {DENOM}: {:?} vs {:?}\",
            test_state.contract_addr,
            balance,
            Uint128::new(trace_balance)
        );
        assert_eq!(balance, Uint128::new(trace_balance));

        // TODO: Query the contract and compare the state as you wish
    }

    fn compare_result(
        trace_result: Result<Response, String>,
        app_result: Result<AppResponse, anyhow::Error>,
    ) {
        if trace_result.is_ok() {
            assert!(
                app_result.is_ok(),
                \"Action unexpectedly failed, error: {:?}\",
                app_result.err()
            );
            println!(\"Action successful as expected\");
        } else {
            assert!(
                app_result.is_err(),
                \"Expected action to fail with error: {:?}\",
                trace_result.err()
            );
            println!(\"Action failed as expected\");
        }
    }

    fn funds_from_trace(amount: Option<BigInt>, denom: Option<String>) -> Vec<cosmwasm_std::Coin> {
        if amount.is_none() || denom.is_none() || amount == Some(Zero::zero()) {
            return vec![];
        }

        vec![coin(
            amount.as_ref().unwrap().to_u128().unwrap(),
            denom.unwrap(),
        )]
    }

    // Testing is stateful.
    struct TestState {
        // we will only know the contract address once we have processed an `instantiate` step
        pub contract_addr: Addr,
    }

    #[test]
    fn model_test() {
        let mut app = App::default();
        let code = ContractWrapper::new(
            crate::contract::execute,
            crate::contract::instantiate,
            crate::contract::query,
        );
        let code_id = app.store_code(Box::new(code));

        // create test state
        let mut test_state = TestState {
            contract_addr: Addr::unchecked(\"contract0\"),
        };

        // load trace data
        let data = include_str!(\"./out/test.itf.json\");
        let trace: itf::Trace<State> = trace_from_str(data).unwrap();

        for s in trace.states {
            let last_result = to_result(s.value.result.clone());
            if last_result.is_ok() && !last_result.unwrap().messages.is_empty() {
                println!(\"Processing messages, skipping\");
                continue;
            }

            let action_taken = &s.value.action_taken;
            let nondet_picks = &s.value.nondet_picks;
            let amount = to_option(nondet_picks.amount.clone());
            let denom = to_option(nondet_picks.denom.clone());
            let sender = to_option(nondet_picks.sender.clone());

            println!(\"Step number: {:?}\", s.meta.index);

            match action_taken.as_str() {
";

pub const TEST_FOOTER: &str = "
                _ => panic!(\"Invalid action taken\"),
            }
            compare_state(&test_state, &app, &(s.value.clone()));
            println!(
                \"clock is advancing for {} seconds (LOCK_PERIOD)\",
                LOCK_PERIOD
            );
            app.update_block(|block| {
                block.time = block.time.plus_seconds(LOCK_PERIOD);
            });
            println!(\"-----------------------------------\");
        }
    }
";
