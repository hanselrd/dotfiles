{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.user.development.rust;
in {
  options = {
    roles.user.development.rust = {
      enable = lib.mkEnableOption "roles.user.development.rust";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      cargo
      rust-cbindgen
      rustc
      rustfmt
      wasm-bindgen-cli
    ];
  };
}
