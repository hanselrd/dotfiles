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

  config = lib.mkIf cfg.enable {
    boot.kernelPackages = pkgs.linuxPackages_latest;
  };
}
