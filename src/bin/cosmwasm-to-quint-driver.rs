fn main() {
  env_logger::init();
  rustc_plugin::driver_main(cosmwasm_to_quint::CosmwasmToQuintPlugin);
}
