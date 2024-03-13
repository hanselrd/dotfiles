{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.user.development.gleam;
in {
  options = {
    roles.user.development.gleam = {
      enable = lib.mkEnableOption "roles.user.development.gleam";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      erlang
      gleam
      rebar3
    ];
  };
}
