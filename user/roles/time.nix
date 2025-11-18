{
  config,
  lib,
  pkgs,
  env,
  ...
}:
let
  cfg = config.roles.user.time;
in
{
  options = {
    roles.user.time = {
      enable = lib.mkEnableOption "roles.user.time";
    };
  };

  config = lib.mkIf cfg.enable {
    home.sessionVariables = {
      TZ = env.roles.user.time.timeZone;
      TZDIR = env.roles.user.time.timeZoneDirectory;
    };
  };
}
