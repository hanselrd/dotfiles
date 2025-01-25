{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.roles.system.xrdp;
in
{
  options = {
    roles.system.xrdp = {
      enable = lib.mkEnableOption "roles.system.xrdp";
    };
  };

  config = lib.mkIf cfg.enable (
    lib.optionalAttrs (!lib.profiles.isSystemDarwin) {
      services.xrdp = {
        enable = true;
        defaultWindowManager = "startplasma-x11";
        openFirewall = true;
      };
    }
  );
}
