{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./minimal.nix
  ];

  home.packages = with pkgs; [
    ffmpeg
    pandoc
    pywal
    tlp
    udisks
    ventoy-bin
  ];
}
