{ pkgs, ... }:
{
  home.packages = with pkgs; [
    # rust-cbindgen
    # wasm-bindgen-cli
    rust-analyzer
    rust-bin.nightly.latest.default
  ];
}
