{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.roles.system.garuda;
in
{
  options = {
    roles.system.garuda = {
      enable = lib.mkEnableOption "roles.system.garuda";
    };
  };

  config = lib.mkIf cfg.enable (
    lib.optionalAttrs lib.profiles.isSystemGaruda {
      garuda = {
        dr460nized.enable = true;
      };
    }
  );
}
