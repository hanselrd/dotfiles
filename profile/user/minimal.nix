{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./standard.nix
  ];

  home.packages = with pkgs; [
    ffmpeg
    pandoc
    pywal
    tlp
    udisks
    ventoy-bin
    xdg-user-dirs
  ];
}
