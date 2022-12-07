{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ../../core/user/role/common/index.nix
    ../../core/user/role/nix/index.nix
    ../../core/user/role/shell/index.nix
    ../../core/user/role/editor/index.nix
    ../../core/user/role/pager/index.nix
    ../../core/user/role/development/common/index.nix
    ../../core/user/role/development/c_cpp/index.nix
    ../../core/user/role/development/dhall/index.nix
    ../../core/user/role/development/elixir/index.nix
    ../../core/user/role/development/go/index.nix
    ../../core/user/role/development/java/index.nix
    ../../core/user/role/development/lua/index.nix
    ../../core/user/role/development/nix/index.nix
    ../../core/user/role/development/nodejs/index.nix
    ../../core/user/role/development/python/index.nix
    ../../core/user/role/development/rust/index.nix
    ../../core/user/role/development/shell/index.nix
    # ../../core/user/role/scripts/index.nix
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
    htop
    lm_sensors
    tree
  ];

  programs.bat = lib.core.user.mkProgram "bat" {};

  programs.exa = lib.core.user.mkProgram "exa" {};

  programs.git = lib.core.user.mkProgram "git" {};

  # programs.gpg = lib.core.user.mkProgram "gpg" {};

  programs.ssh = lib.core.user.mkProgram "ssh" {};

  programs.tmux = lib.core.user.mkProgram "tmux" {};

  # services.gpg-agent = lib.core.user.mkService "gpg-agent" {};
}
