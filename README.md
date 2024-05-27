<p align="center">
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="./images/cosmwasm-to-quint-light.png">
    <img alt="CoswmWasm to Quint" src="./images/cosmwasm-to-quint-dark.png" width=700>
  </picture>
</p>

*Semi-automated modelling and Model-Based Testing for [CosmWasm](https://cosmwasm.com/) contracts.*
1. Generate [Quint](https://github.com/informalsystems/quint) model stubs for a
   contract, and only worry about specifying the important bits - the
   entrypoints.
2. Generate tests to check that the model corresponds to the implementation by
   attempting to reproduce traces.

Quint provides powerful and friendly simulation and verification tools, but
first you need a model. This project **speeds up** the process of getting a model
for CosmWasm contracts. And, as a bonus, you can get as many integration tests
as you want, including tests to specific scenarios you deem important, through
**generated Model-Based tests**.

This can be used to find bugs using the Quint tools and then getting them
reproduced in the real implementation with a generated test. For a better
understanding of how this whole process works, take a look at our [AwesomWasm
2024 Workshop
material](https://github.com/informalsystems/quint_awesomwasm24_workshop).

## Is this ready to be used?

Yes!

Even though this tool is a prototype, it will make the best attempt to
generate as much as it can. It adds placeholders like `<missing-type>` to parts
it can't translate, which you then have to replace manually. But it is
definitely better than writing everything from scratch!

See section [Known Limitations](#known-limitations) for problems to expect.

## Examples

If you want to see examples of generated code without running the tool yourself,
take a look at the [`tests/snapshots`
folder](https://github.com/informalsystems/cosmwasm-to-quint/tree/main/tests/snapshots).
We use [Oak Security's Capture the Flag
challenges](https://github.com/oak-security/cosmwasm-ctf) as tests, and these
snapshots contain both the model stubs and rust tests generated for each Capture
the Flag (CTF) challenge.

## Setup and generation

This project is implemented as a rust compiler plugin, in order to easily access
everything it needs from your CosmWasm contract. Because of that, we need a
couple extra steps in our setup, which in overview looks like this:
1. Install this tool
2. Use the nightly version of the rust compiler 
3. Update your project's dependencies
4. Run the tool

See detailed instructions below.

### Installation

To compile and install this project, clone this repo and run:

```bash
# Install the cosmwasm-to-quint binaries
cargo install --path .
```

### Setup

Running this tool for a CosmWasm contracts requires a simple `cargo` command,
but you need to set some things up first.

1. Go to the directory of your CosmWasm project

``` bash
cd /path/to/my-cosmwasm-project
```

2. Setup `rustup` to use a `nightly` version compatible with compiler plugin
libraries, and add the necessary components and target. **Important**: You also
need to update your project dependencies (`cargo update`) to match the new tool
chain, otherwise compilation will likely fail:

``` bash
# Install and use a nightly version
rustup toolchain add nightly-2024-01-06
rustup override set nightly-2024-01-06
# Add components and target
rustup component add clippy rust-src rustc-dev llvm-tools-preview
rustup target add wasm32-unknown-unknown
# Update dependencies on Cargo.lock to match the new tool chain
cargo update
```


### Generating the files

Since the translation is called by compiler callbacks, you need to make sure
that the compiler is actually called. For that, remove any previous compilation
results by calling `cargo clean` before executing this project's binary by
calling `cargo cosmwasm-to-quint`

``` bash
cargo clean && cargo cosmwasm-to-quint
```

### Running the generated tests

1. In order to run the generated tests, you'll need to add some dependencies:
```bash
cargo add cw-multi-test@0.16.2 --dev
cargo add itf@0.2.4 --dev
cargo add anyhow@1.0.83 --dev
cargo add num-bigint@0.4.4 --dev
cargo add num-traits@0.2.17 --dev
```

2. The tests also require a trace. By default, it will look for the
   `quint/test.itf.json` file, but you can change this path in the test file.
   The trace is some execution from the Quint model, which the test will attempt
   to reproduce in your contract. You should always use the `--mbt` flag to tell
   Quint to include test-relevant information to the trace (this option is
   available from Quint `v0.20.0`).

There are a number of ways to obtain a trace, but the simplest one is:

``` bash
quint run quint/my_model.qnt --mbt --out-itf=quint/test.itf.json
```

Alternatively, if you have an invariant that is being violated in the model and
want to reproduce a counterexample of that invariant in the contract, simply
provide the invariant to the command:

``` bash
quint run quint/my_model.qnt --mbt --out-itf=quint/test.itf.json --invariant=my_invariant
```

You can also use this to obtain interesting traces. Say you want a test where
Alice spends all of her ATOMs. You can write an invariant like
`balances.get("Alice").get("ATOM") > 0` and then use it to obtain a trace
violating that invariant.

3. Run the tests:

``` bash
cargo test
```

## Known limitations
- **Imports from different crates**: this is something we want to support, but
  it is not trivial. Right now, if your contract crate imports core data
  structures or other items that are necessary for the generation, you'll have
  unresolved names and placeholders to fill for each of them. Of course, this
  doesn't apply to common imports from `cosmwasm_std` which are already dealt
  with.
- **Multiple contract interactions**: this is probably the next important step
  for this project, but we are not there yet. If you need to model and test
  interactions between multiple contracts, you will need to do a lot of the
  wiring manually. Fortunately, we believe it is possible to automate this, so
  let us know if this would be useful for your project!

## Troubleshooting
1. I see an "unknown feature `proc_macro_span_shrink`" error

You probably forgot to run `cargo update` after switching the tool chain

2. Running `cargo cosmwasm-to-quint` exits immediately and doesn't
   produce/update any files

You probably forgot to run `cargo clean` before running the command

3. I'm getting duplicated definitions for a bunch of things

Make sure you have **not** added a generated test file to your contract's crate
somehow. If you do that, this tool will read the test file as it was part of
your contract and, therefore, generate duplicated items.
