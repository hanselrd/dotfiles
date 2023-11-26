{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./standard.nix
  ];

  roles.user.browser.enable = true;
  roles.user.terminal.enable = true;

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
