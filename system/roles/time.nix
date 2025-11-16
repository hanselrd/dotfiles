{
  config,
  lib,
  pkgs,
  env,
  ...
}:
let
  cfg = config.roles.system.time;
in
{
  options = {
    roles.system.time = {
      enable = lib.mkEnableOption "roles.system.time";
    };
  };

  config = lib.mkIf cfg.enable {
    time.timeZone = env.roles.system.time.timeZone;
  };
}
