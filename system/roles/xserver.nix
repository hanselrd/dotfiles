{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.roles.system.xserver;
in
{
  options = {
    roles.system.xserver = {
      enable = lib.mkEnableOption "roles.system.xserver";
    };
  };

  config = lib.mkIf cfg.enable (
    lib.optionalAttrs (!lib.profiles.isSystemDarwin) {
      services.xserver = {
        # enable = true;
        xkb = {
          layout = "us";
          variant = "";
        };
      };
    }
  );
}
