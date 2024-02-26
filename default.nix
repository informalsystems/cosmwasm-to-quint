with (import <nixpkgs> { });
let
  shell = mkShell {
    name = "rust-env";
    buildInputs = [ pkgs.rustup pkgs.rustc pkgs.rust-analyzer pkgs.rustic-rs ];
    shellHook = ''
      rustup toolchain add nightly-2024-01-06
      rustup override set nightly-2024-01-06
      rustup component add clippy rust-src rustc-dev llvm-tools-preview
      rustup target add wasm32-unknown-unknown
    '';
  };
in shell
