{
  config,
  lib,
  pkgs,
  env,
  ...
}:
{
  programs.btop = {
    enable = true;
    settings = {
      clock_format = lib.replaceStrings [ "%-" ] [ "%" ] env.timeFormat;
      update_ms = 3000;
    };
  };
}
