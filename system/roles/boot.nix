{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.system.boot;
in {
  options = {
    roles.system.boot = {
      enable = lib.mkEnableOption "roles.system.boot";
    };
  };

  config = lib.mkIf cfg.enable {
    boot.loader.systemd-boot.enable = true;
    boot.loader.efi.canTouchEfiVariables = true;
    boot.loader.efi.efiSysMountPoint = "/boot/efi";

    boot.tmp.cleanOnBoot = true;
  };
}
