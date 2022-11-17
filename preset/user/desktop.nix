{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./minimal.nix
    ../../core/user/role/browser/index.nix
  ];

  home.packages = with pkgs; [
    # arandr
    # bolt
    # clang
    # cups
    # feh
    # ffmpeg
    # flameshot
    # lxappearance
    # nerd-fonts
    # noto-fonts
    # noto-fonts-cjk-sans
    # noto-fonts-cjk-serif
    # noto-fonts-emoji
    # noto-fonts-emoji-blob-bin
    # noto-fonts-extra
    # numix-gtk-theme
    # numix-icon-theme
    # numix-icon-theme-circle
    # numix-icon-theme-square
    # openresolv
    # openvpn
    # picom
    # redshift
    # renderdoc
    # rofi
    # snapper
    # tlp
    # udisks
    # upower
    # ventoy-bin
    # w3m
    # xdg-user-dirs
    # xsel
    brave
    dbeaver
    jetbrains-mono
    liberation_ttf
    libreoffice-fresh
    zoom-us
  ];
}
