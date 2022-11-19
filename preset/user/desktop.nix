{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./minimal.nix
    ../../core/user/role/terminal/index.nix
    ../../core/user/role/browser/index.nix
  ];

  home.packages = with pkgs; [
    arandr
    dbeaver
    flameshot
    jetbrains-mono
    liberation_ttf
    libreoffice-fresh
    nerd-fonts
    noto-fonts
    noto-fonts-cjk-sans
    noto-fonts-cjk-serif
    noto-fonts-emoji
    noto-fonts-emoji-blob-bin
    noto-fonts-extra
    redshift
    renderdoc
    rofi
    zoom-us
  ];
}
