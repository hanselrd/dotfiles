{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.roles.user.redshift;
in
{
  options = {
    roles.user.redshift = {
      enable = lib.mkEnableOption "roles.user.redshift";
    };
  };

  config = lib.mkIf cfg.enable {
    services.redshift = {
      enable = true;
      temperature.day = 5500;
      temperature.night = 2500;
      provider = "geoclue2";
    };
  };
}
