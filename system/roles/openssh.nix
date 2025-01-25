{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.roles.system.openssh;
in
{
  options = {
    roles.system.openssh = {
      enable = lib.mkEnableOption "roles.system.openssh";
    };
  };

  config = lib.mkIf cfg.enable (
    lib.optionalAttrs (!lib.profiles.isSystemDarwin) {
      services.openssh = {
        enable = true;
        settings = {
          X11Forwarding = config.roles.system.xserver.enable;
        };
      };
    }
  );
}
