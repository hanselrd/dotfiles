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
    ../../core/user/role/terminal/index.nix
  ];

  home.packages = with pkgs; [
    # arandr
    # bolt
    # brave
    # clang
    # cups
    # dbeaver
    # feh
    # ffmpeg
    # flameshot
    # jetbrains-mono
    # liberation_ttf
    # libreoffice-fresh
    # lxappearance
    # nerd-fonts
    # noto-fonts
    # noto-fonts-cjk-sans
    # noto-fonts-cjk-serif
    # noto-fonts-emoji
    # noto-fonts-emoji-blob-bin
    # noto-fonts-extra
    # numix-gtk-theme
    # numix-icon-theme
    # numix-icon-theme-circle
    # numix-icon-theme-square
    # openresolv
    # openvpn
    # picom
    # redshift
    # renderdoc
    # rofi
    # snapper
    # tlp
    # udisks
    # upower
    # ventoy-bin
    # w3m
    # xdg-user-dirs
    # xsel
    # zoom-us

    # go
    # htop
    # jq
    # lazygit
    # pandoc
    # pywal
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
    fortune
    gcc
    gdb
    git-crypt
    glfw
    gnumake
    gnupatch
    graphviz
    httpie
    imagemagick
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

  programs.bat = lib.ext.mkProgram "bat" {};

  programs.exa = lib.ext.mkProgram "exa" {};

  programs.git = lib.ext.mkProgram "git" {};

  programs.gpg = lib.ext.mkProgram "gpg" {};

  programs.ssh = lib.ext.mkProgram "ssh" {};

  programs.tmux = lib.ext.mkProgram "tmux" {};

  programs.zoxide = lib.ext.mkProgram "zoxide" {};

  services.gpg-agent = lib.ext.mkService "gpg-agent" {};
}
