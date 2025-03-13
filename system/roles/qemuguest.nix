{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.roles.system.qemuguest;
in
{
  options = {
    roles.system.qemuguest = {
      enable = lib.mkEnableOption "roles.system.qemuguest";
    };
  };

  config = lib.mkIf cfg.enable (
    lib.optionalAttrs (!lib.profiles.isSystemDarwin) {
      services.qemuGuest.enable = true;
    }
  );
}
