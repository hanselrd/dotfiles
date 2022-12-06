{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./server.nix
    ../../core/user/role/docker/index.nix
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
