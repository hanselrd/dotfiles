{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./base.nix
  ];

  roles.user.development.enable = true;
  roles.user.docker.enable = true;

  home.packages = with pkgs; [
    # SDL2
    # SDL2_image
    # SDL2_mixer
    # SDL2_net
    # SDL2_ttf
    # android-tools
    # ansible
    # atool
    # fd
    # figlet
    # flock
    # fortune
    # git-crypt
    # glfw
    # graphviz
    # httpie
    # imagemagick
    # jq
    # lolcat
    # mesa
    # mesa-demos
    # neofetch
    # nmap
    # objconv
    # openvpn
    # perl
    # postgresql
    # pre-commit
    # ranger
    # restic
    # ripgrep
    # rsync
    # ruby
    # sshfs
    # stow
    # strace
    # tokei
    # vulkan-tools
    # wtf
    # xsv
    # youtube-dl
    # zlib
    # zstd
    btop
    lm_sensors
  ];
}
