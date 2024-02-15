# WIP - cosmwasm-to-quint

This uses the crate `rustc_plugin` to generate quint code out of cosmwasm projects

```bash
# Install the cosmwasm-to-quint binaries
cargo install --path .

# Run the binaries on an example crate
cd test-crate
cargo cosmwasm-to-quint
```

PS: For now, you might need to remove the `target` folder to prevent the
compilers from reusing targets and, therefore, not calling the necessary
compiler callbacks.
