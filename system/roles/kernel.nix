{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.system.kernel;
in {
  options = {
    roles.system.kernel = {
      enable = lib.mkEnableOption "roles.system.kernel";
    };
  };

  config = lib.mkIf cfg.enable (
    lib.optionalAttrs (!lib.profiles.isSystemDarwin) {
      boot.kernelPackages = pkgs.linuxPackages_cachyos-rc;

      chaotic.scx = {
        enable = true;
        scheduler = "scx_lavd";
      };

      boot.kernel.sysctl = {
        "vm.swappiness" = 1;
        "vm.vfs_cache_pressure" = 500;
      };
    }
  );
}
