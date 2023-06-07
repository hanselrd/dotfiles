{
  config,
  lib,
  pkgs,
  ...
}: {
  home.packages = with pkgs; [
    rust-cbindgen
    rustup
    wasm-bindgen-cli
  ];
}
