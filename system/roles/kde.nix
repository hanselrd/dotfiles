{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.roles.system.kde;
in
{
  options = {
    roles.system.kde = {
      enable = lib.mkEnableOption "roles.system.kde";
    };
  };

  config = lib.mkIf cfg.enable (
    lib.optionalAttrs (!lib.profiles.isSystemDarwin) {
      services.displayManager.sddm.enable = true;
      services.desktopManager.plasma6.enable = true;
    }
  );
}
