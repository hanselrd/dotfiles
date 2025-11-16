{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.roles.user.development.nodejs;
in
{
  options = {
    roles.user.development.nodejs = {
      enable = lib.mkEnableOption "roles.user.development.nodejs";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      bun
      nodejs
    ];

    home.sessionVariables = {
      NEXT_TELEMETRY_DISABLED = 1;
    };
  };
}
