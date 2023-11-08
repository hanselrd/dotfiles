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
    # jetbrains-mono
    # nerdfonts
    (nerdfonts.override {fonts = ["JetBrainsMono"];})
    arandr
    dbeaver
    flameshot
    liberation_ttf
    libreoffice-fresh
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
