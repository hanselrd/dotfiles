{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.system.udisks2;
in {
  options = {
    roles.system.udisks2 = {
      enable = lib.mkEnableOption "roles.system.udisks2";
    };
  };

  config = lib.mkIf cfg.enable (
    lib.optionalAttrs (!lib.profiles.isSystemDarwin) {
      services.udisks2 = {
        enable = true;
        mountOnMedia = true;
      };
    }
  );
}
