{
  config,
  lib,
  pkgs,
  ...
}:
{
  services.redshift = {
    enable = true;
    temperature.day = 5500;
    temperature.night = 2500;
    provider = "geoclue2";
  };
}
