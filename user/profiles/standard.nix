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
    # pywal
    ffmpeg
    pandoc
    tlp
    udisks
    ventoy-bin
  ];
}
