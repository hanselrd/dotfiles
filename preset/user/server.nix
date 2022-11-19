{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ../../core/user/role/base/index.nix
    ../../core/user/role/nix/index.nix
    ../../core/user/role/shell/index.nix
    ../../core/user/role/editor/index.nix
    ../../core/user/role/pager/index.nix
    ../../core/user/role/scripts/index.nix
  ];

  home.packages = with pkgs; [
    age
    alejandra
    android-tools
    ansible
    atool
    ccache
    clang-tools
    cmake
    ctags
    dhall
    dhall-bash
    dhall-json
    dhall-lsp-server
    dhall-nix
    docker
    docker-compose
    elixir
    emscripten
    fd
    figlet
    flock
    fortune
    gcc
    gdb
    git-crypt
    glfw
    gnumake
    gnupatch
    go
    graphviz
    htop
    httpie
    imagemagick
    jq
    lldb
    llvm
    lolcat
    lua
    mesa
    mesa-demos
    meson
    neofetch
    ninja
    nixpkgs-fmt
    nmap
    nodejs
    objconv
    openvpn
    perl
    postgresql
    pre-commit
    python3
    ranger
    restic
    ripgrep
    rsync
    ruby
    rustup
    SDL2
    SDL2_image
    SDL2_mixer
    SDL2_net
    SDL2_ttf
    shellcheck
    shfmt
    sshfs
    stow
    strace
    tokei
    tree
    vulkan-tools
    wtf
    xsv
    youtube-dl
    zlib
    zstd
  ];

  programs.bat = lib.core.user.mkProgram "bat" {};

  programs.exa = lib.core.user.mkProgram "exa" {};

  programs.git = lib.core.user.mkProgram "git" {};

  programs.gpg = lib.core.user.mkProgram "gpg" {};

  programs.ssh = lib.core.user.mkProgram "ssh" {};

  programs.tmux = lib.core.user.mkProgram "tmux" {};

  programs.zoxide = lib.core.user.mkProgram "zoxide" {};

  # services.gpg-agent = lib.core.user.mkService "gpg-agent" {};
}
