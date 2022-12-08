{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [./server.nix];

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
