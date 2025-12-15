{
  inputs,
  config,
  lib,
  pkgs,
  sharedModulesPath,
  env,
  ...
}:
{
  imports = with inputs; [
    agenix.homeManagerModules.default
    impermanence.homeManagerModules.impermanence
    stylix.homeModules.stylix
    (sharedModulesPath + "/common")
    ../binaries
    ../packages/base
    ../programs/bash
    ../programs/bat
    ../programs/btop
    ../programs/ccache
    ../programs/cmake
    ../programs/delta
    ../programs/eza
    ../programs/fastfetch
    ../programs/fzf
    ../programs/gdb
    ../programs/git
    ../programs/home-manager
    ../programs/htop
    ../programs/lldb
    ../programs/neovim
    ../programs/nh
    ../programs/nix-index
    ../programs/oh-my-posh
    ../programs/ranger
    ../programs/ripgrep
    ../programs/ssh
    ../programs/tmux
    ../programs/zoxide
    ../programs/zsh
  ];

  home.username = env.username;
  home.homeDirectory = env.homeDirectory;

  home.preferXdgDirectories = true;

  xdg.enable = true;

  # xdg.userDirs = {
  #   enable = true;
  #   createDirectories = true;
  # };

  home.language.base = "en_US.UTF-8";

  home.shellAliases = {
    cd1 = "cd ..";
    cd2 = "cd ../..";
    cd3 = "cd ../../..";
    cd4 = "cd ../../../..";
    cd5 = "cd ../../../../..";
    rcp = "rsync -CcavzP";
    rmv = "rsync -CcavzP --remove-source-files";
    shroot = "sudo -E $SHELL";
    sudo = "sudo ";
    vi = "vim -u NONE -U NONE -N -i NONE";
  };

  home.sessionVariables = rec {
    BROWSER = "brave";

    CS_DISABLE_FILE_DOWNLOADS = 1;
    EDITOR = "nvim";
    SUDO_EDITOR = EDITOR;
    VISUAL = EDITOR;

    PAGER = "less -s";
    MANPAGER = PAGER;

    TERMINAL = "alacritty";

    TZ = env.timeZone;
    TZDIR = "\${TZDIR:-/usr/share/zoneinfo}";

    HISTTIMEFORMAT = "${env.timeFormat}  ";
    # LD_LIBRARY_PATH = "$LD_LIBRARY_PATH\${LD_LIBRARY_PATH:+:}${pkgs.sssd}/lib";
  };

  nix.nixPath = [ "${config.xdg.configHome}/nix/path" ];
  xdg.configFile = lib.mapAttrs' (name: value: {
    name = "nix/path/${name}";
    value.source = value.flake;
  }) config.nix.registry;
}
