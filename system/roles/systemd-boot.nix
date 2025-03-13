{
  config,
  lib,
  pkgs,
  env,
  ...
}:
let
  cfg = config.roles.system.systemd-boot;
in
{
  options = {
    roles.system.systemd-boot = {
      enable = lib.mkEnableOption "roles.system.systemd-boot";
    };
  };

  config = lib.mkIf cfg.enable (
    lib.optionalAttrs (!lib.profiles.isSystemDarwin) {
      boot.loader.systemd-boot = {
        enable = true;
        xbootldrMountPoint = lib.mkIf env.roles.system.systemd-boot.xbootldr "/boot";
      };

      boot.loader.efi.efiSysMountPoint =
        if env.roles.system.systemd-boot.xbootldr then "/efi" else "/boot/efi";
    }
  );
}
