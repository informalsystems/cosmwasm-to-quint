# CosmWasm to Quint

This uses the crate `rustc_plugin` to generate quint code out of cosmwasm projects. To compile it, clone this repo and run:

```bash
# Install the cosmwasm-to-quint binaries
cargo install --path .
```

Running this tool for a CosmWasm contracts requires a simple `cargo` command, but you need to set some things up first. Make sure you are on the directory of your CosmWasm project and follow these steps:

1. Setup `rustup` to use a `nightly` version compatible with compiler plugin libs

``` bash
rustup toolchain add nightly-2024-01-06
rustup override set nightly-2024-01-06
rustup component add clippy rust-src rustc-dev llvm-tools-preview
rustup target add wasm32-unknown-unknown
```


2. Because of this changes on the toolchain, you might need to remove any `Cargo.lock` files. Otherwise, you might hit a "unknown feature `proc_macro_span_shrink`" error.

3. Since the translation is called by compiler callbacks, you need to make sure that the compiler is actually called. For that, remove any previous compilation results by calling `cargo clean` before executing this project's binary by calling `cargo cosmwasm-to-quint`

``` bash
cargo clean && cargo cosmwasm-to-quint
```

4. In order to run the generated tests, you'll need to add some dependencies:
```bash
cargo add cw-multi-test@0.16.2 --dev
cargo add itf@0.2.4 --dev
cargo add anyhow@1.0.83 --dev
cargo add num-bigint@0.4.4 --dev
cargo add num-traits@0.2.17 --dev
```

5. TODO: Add instructions on how to produce traces for the test
