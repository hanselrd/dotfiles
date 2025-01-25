{
  config,
  lib,
  pkgs,
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
    time.timeZone = "America/New_York";
  };
}
