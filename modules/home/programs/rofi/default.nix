{
  config,
  lib,
  pkgs,
  ...
}:
{
  programs.rofi = {
    enable = true;
    # TODO: font/theme
  };

  stylix.targets.rofi.enable = true;
}
