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

  config = lib.mkIf cfg.enable (
    lib.optionalAttrs (!lib.profiles.isSystemDarwin) {
      roles.system.openssh.enable = true;

      services.cockpit = {
        enable = true;
        openFirewall = true;
      };
    }
  );
}
