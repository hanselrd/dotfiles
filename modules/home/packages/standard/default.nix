{
  config,
  lib,
  pkgs,
  ...
}:
{
  imports = [ ../base ];

  home.packages = with pkgs; [
    # ventoy
    (lib.hiPrio stress)
    android-tools
    ansible
    atool
    cowsay
    docker
    docker-compose
    fd
    ffmpeg
    figlet
    flock
    fortune
    graphviz
    httpie
    hyperfine
    imagemagick
    lolcat
    nmap
    restic
    speedtest-cli
    sshfs
    stow
    tlp
    tokei
    tshark
    udisks
    wireguard-tools
    yt-dlp
  ];
}
