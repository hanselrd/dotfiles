{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.roles.system.boot;
in
{
  options = {
    roles.system.boot = {
      enable = lib.mkEnableOption "roles.system.boot";
    };
  };

  config = lib.mkIf cfg.enable (
    lib.recursiveUpdate
      {
        roles.system.grub.enable = true;
        # roles.system.systemd-boot.enable = true;
      }
      (
        lib.optionalAttrs (!lib.profiles.isSystemDarwin) {
          boot.loader.efi.canTouchEfiVariables = true;

          boot.tmp.cleanOnBoot = true;
        }
      )
  );
}
