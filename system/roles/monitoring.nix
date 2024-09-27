{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.system.monitoring;
in {
  options = {
    roles.system.monitoring = {
      enable = lib.mkEnableOption "roles.system.monitoring";
    };
  };

  config = lib.mkIf cfg.enable {
    roles.system.cockpit.enable = true;
    roles.system.openssh.enable = true;
  };
}
