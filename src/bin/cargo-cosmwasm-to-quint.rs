fn main() {
  env_logger::init();
  rustc_plugin::cli_main(cosmwasm_to_quint::CosmwasmToQuintPlugin);
}
