{
  config,
  lib,
  pkgs,
  ...
}:
{
  imports = [ ../standard ];

  home.packages = with pkgs; [
    arandr
    bottles
    brave
    dbeaver-bin
    heroic
    libreoffice-fresh
    lutris
    mesa
    mesa-demos
    protonup-qt
    renderdoc
    vulkan-tools
    wineWowPackages.stableFull
    winetricks
    zoom-us
  ];
}
