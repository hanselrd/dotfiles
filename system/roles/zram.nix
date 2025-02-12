{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.roles.system.zram;
in
{
  options = {
    roles.system.zram = {
      enable = lib.mkEnableOption "roles.system.zram";
    };
  };

  config = lib.mkIf cfg.enable (
    lib.optionalAttrs (!lib.profiles.isSystemDarwin) {
      zramSwap.enable = true;
    }
  );
}
