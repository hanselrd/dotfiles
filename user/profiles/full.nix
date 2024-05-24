{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./standard.nix
  ];

  # roles.user.flameshot.enable = true;
  # roles.user.redshift.enable = true;
  # roles.user.rofi.enable = true;
  roles.user.browser.enable = true;
  roles.user.terminal.enable = true;
  roles.user.vscode.enable = true;

  home.packages = with pkgs; [
    (nerdfonts.override {fonts = ["JetBrainsMono"];})
    arandr
    dbeaver-bin
    liberation_ttf
    libreoffice-fresh
    mesa
    mesa-demos
    noto-fonts
    noto-fonts-cjk-sans
    noto-fonts-cjk-serif
    noto-fonts-emoji
    noto-fonts-emoji-blob-bin
    noto-fonts-extra
    renderdoc
    vulkan-tools
    zoom-us
  ];
}
