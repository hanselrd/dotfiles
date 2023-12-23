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
      pkgs.rust-bin.nightly.latest.default
      # rust-cbindgen
      # wasm-bindgen-cli
    ];
  };
}
