{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.system.cockpit;
in {
  options = {
    roles.system.cockpit = {
      enable = lib.mkEnableOption "roles.system.cockpit";
    };
  };

  config = lib.mkIf cfg.enable (
    lib.optionalAttrs (!lib.profiles.isSystemDarwin) {
      services.cockpit = {
        enable = true;
        openFirewall = true;
      };
    }
  );
}
