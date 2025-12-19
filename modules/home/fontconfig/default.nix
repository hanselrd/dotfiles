{
  config,
  lib,
  pkgs,
  ...
}:
{
  fonts.fontconfig.enable = true;

  home.packages = with pkgs; [
    liberation_ttf
    nerd-fonts.jetbrains-mono
    noto-fonts
    noto-fonts-cjk-sans
    noto-fonts-cjk-serif
    noto-fonts-color-emoji
    noto-fonts-lgc-plus
  ];
}
