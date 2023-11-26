{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.user.development.dhall;
in {
  options = {
    roles.user.development.dhall = {
      enable = lib.mkEnableOption "roles.user.development.dhall";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      dhall
      dhall-bash
      dhall-json
      dhall-nix
    ];
  };
}
