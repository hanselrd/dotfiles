{
  config,
  lib,
  pkgs,
  env,
  ...
}:
let
  cfg = config.roles.user.btop;
in
{
  options = {
    roles.user.btop = {
      enable = lib.mkEnableOption "roles.user.btop";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.btop = {
      enable = true;
      settings = {
        clock_format = lib.replaceStrings [ "%-" ] [ "%" ] env.extra.timeFormat;
        update_ms = 3000;
      };
    };
  };
}
