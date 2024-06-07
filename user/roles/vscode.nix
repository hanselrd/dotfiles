{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.user.vscode;
in {
  options = {
    roles.user.vscode = {
      enable = lib.mkEnableOption "roles.user.vscode";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.vscode = {
      enable = true;
      extensions = with pkgs.vscode-extensions; [
        # agurodriguez.vscode-lbnf
        # cschlosser.doxdocgen
        # EditorConfig.EditorConfig
        # IronGeek.vscode-env
        # jeff-hykin.better-cpp-syntax
        # jkiviluoto.tws
        # joaompinto.vscode-graphviz
        # josef.rouge-theme
        # mnxn.lalrpop-highlight
        # ms-vscode-remote.remote-containers
        # ms-vscode-remote.remote-ssh-edit
        # ms-vscode-remote.remote-wsl
        # ms-vscode-remote.vscode-remote-extensionpack
        # ms-vscode.cpptools-extension-pack
        # ms-vscode.cpptools-themes
        # mvakula.vscode-purty
        # nwolverson.ide-purescript
        # nwolverson.language-purescript
        # PKief.material-icon-theme
        # PolyMeilex.wgsl
        # wmaurer.change-case
        # wwm.better-align
        # xmonader.vscode-capnp
        bbenoist.nix
        bungcip.better-toml
        dhall.dhall-lang
        dhall.vscode-dhall-lsp-server
        eamodio.gitlens
        haskell.haskell
        justusadam.language-haskell
        rust-lang.rust-analyzer
        ms-azuretools.vscode-docker
        ms-python.python
        ms-python.vscode-pylance
        ms-toolsai.jupyter
        # ms-toolsai.jupyter-keymap
        ms-toolsai.jupyter-renderers
        ms-vscode-remote.remote-ssh
        # ms-vscode.cmake-tools
        ms-vscode.cpptools
        naumovs.color-highlight
        # twxs.cmake
        vscodevim.vim
        zhuangtongfa.material-theme
      ];
      userSettings = {};
    };
  };
}
