{
  config,
  lib,
  pkgs,
  ...
}:
{
  services.scx = {
    enable = true;
    scheduler = "scx_lavd";
  };
}
