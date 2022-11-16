{
  config,
  lib,
  pkgs,
  ...
}: {
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

    # age
    # android-tools
    # ansible
    # atool
    # ccache
    # clang-tools
    # cmake
    # ctags
    # dhall
    # dhall-bash
    # dhall-json
    # dhall-lsp-server
    # dhall-nix
    # docker
    # docker-compose
    # elixir
    # emscripten
    # fd
    # figlet
    # fortune
    # gcc
    # gdb
    # git-crypt
    # glfw
    # gnumake
    # gnutar
    # go
    # graphviz
    # htop
    # httpie
    # imagemagick
    # jq
    # lazygit
    # lldb
    # llvm
    # lolcat
    # lua
    # mesa
    # mesa-demos
    # meson
    # neofetch
    # ninja
    # nmap
    # nodejs
    # objconv
    # openssh
    # pandoc
    # perl
    # postgresql
    # pre-commit
    # python3
    # pywal
    # ranger
    # restic
    # ripgrep
    # rsync
    # ruby
    # rustup
    # SDL2
    # SDL2_image
    # SDL2_mixer
    # SDL2_net
    # SDL2_ttf
    # sshfs
    # stow
    # strace
    # tokei
    # tree
    # unzip
    # vulkan-tools
    # wtf
    # xsv
    # youtube-dl
    # zlib
    # zstd
    alejandra
    nixpkgs-fmt
    shellcheck
    shfmt
  ];

  imports = [
    ../../core/user/role/base/index.nix
    ../../core/user/role/nix/index.nix
    ../../core/user/role/shell/index.nix
    ../../core/user/role/editor/index.nix
    ../../core/user/role/pager/index.nix
    ../../core/user/role/terminal/index.nix
    # ../../core/user/role/browser/index.nix
  ];

  programs.bat = lib.ext.mkProgram "bat" {};

  programs.exa = lib.ext.mkProgram "exa" {};

  programs.git = lib.ext.mkProgram "git" {};

  programs.gpg = lib.ext.mkProgram "gpg" {};

  programs.ssh = lib.ext.mkProgram "ssh" {};

  programs.tmux = lib.ext.mkProgram "tmux" {};

  programs.zoxide = lib.ext.mkProgram "zoxide" {};

  services.gpg-agent = lib.ext.mkService "gpg-agent" {};

  # homeage = {
  #   identityPaths = [
  #     "${config.home.homeDirectory}/.keys/master.age"
  #     "${config.home.homeDirectory}/.ssh/id_ed25519"
  #   ];

  #   installationType = "activation";

  #   mount = "${config.home.homeDirectory}/.secrets/raw";

  #   file."data" = {
  #     source = ./secrets/data.nix.age;
  #     symlinks = ["${config.home.homeDirectory}/.secrets/data.nix"];
  #   };
  # };
}
