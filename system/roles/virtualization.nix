{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.system.virtualization;
in {
  options = {
    roles.system.virtualization = {
      enable = lib.mkEnableOption "roles.system.virtualization";
    };
  };

  config = lib.mkIf cfg.enable {
    virtualisation.docker.enable = true;
  };
}
