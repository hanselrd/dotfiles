{
  config,
  lib,
  pkgs,
  env,
  ...
}:
let
  cfg = config.roles.system.grub;
in
{
  options = {
    roles.system.grub = {
      enable = lib.mkEnableOption "roles.system.grub";
    };
  };

  config = lib.mkIf cfg.enable (
    lib.optionalAttrs (!lib.profiles.isSystemDarwin) {
      boot.loader.grub = {
        enable = true;
        device = env.roles.system.grub.device;
        useOSProber = true;
      };
    }
  );
}
