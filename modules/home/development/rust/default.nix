{
  config,
  lib,
  pkgs,
  ...
}:
{
  home.packages = with pkgs; [
    rust-bin.nightly.latest.default
    # rust-cbindgen
    # wasm-bindgen-cli
  ];
}
