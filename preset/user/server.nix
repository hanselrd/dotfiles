{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ../../core/user/role/common/index.nix
    ../../core/user/role/nix/index.nix
    ../../core/user/role/theme/index.nix
    ../../core/user/role/shell/index.nix
    ../../core/user/role/editor/index.nix
    ../../core/user/role/pager/index.nix
    ../../core/user/role/docker/index.nix
    ../../core/user/role/development/index.nix
    # ../../core/user/role/scripts/index.nix
    ../../core/user/role/other/index.nix
  ];

  home.packages = with pkgs; [
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
    # SDL2
    # SDL2_image
    # SDL2_mixer
    # SDL2_net
    # SDL2_ttf
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
    age
    btop
    htop
    lm_sensors
    tree
    wget
  ];
}
