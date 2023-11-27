{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./minimal.nix
  ];

  # roles.user.jq.enable = true;
  # roles.user.pandoc.enable = true;

  home.packages = with pkgs; [
    android-tools
    ansible
    atool
    fd
    ffmpeg
    figlet
    flock
    fortune
    graphviz
    httpie
    imagemagick
    lolcat
    nmap
    ranger
    restic
    sshfs
    stow
    tlp
    tokei
    udisks
    ventoy-bin
    wtf
    xsv
    youtube-dl
    zlib
    zstd
  ];
}
