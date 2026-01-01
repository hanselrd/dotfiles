{ pkgs, ... }:
{
  programs.vscode = {
    enable = true;
    profiles.default.extensions = with pkgs.vscode-extensions; [
      # agurodriguez.vscode-lbnf
      # chouzz.vscode-better-align
      # cschlosser.doxdocgen
      # eliverlara.andromeda
      # guyutongxue.lalrpop-syntax-highlight
      # jeff-hykin.better-cpp-syntax
      # jkiviluoto.tws
      # joaompinto.vscode-graphviz
      # josef.rouge-theme
      # ms-vscode-remote.vscode-remote-extensionpack
      # ms-vscode.cpptools-themes
      # ms-vscode.remote-explorer
      # ms-vscode.remote-server
      # mvakula.vscode-purty
      # nwolverson.ide-purescript
      # nwolverson.language-purescript
      # polymeilex.wgsl
      # rust-lang.rust-analyzer
      # wayou.vscode-todo-highlight
      # xmonader.vscode-capnp
      bbenoist.nix
      dhall.dhall-lang
      dhall.vscode-dhall-lsp-server
      eamodio.gitlens
      editorconfig.editorconfig
      haskell.haskell
      irongeek.vscode-env
      justusadam.language-haskell
      ms-azuretools.vscode-docker
      # ms-python.black-formatter
      # ms-python.debugpy
      # ms-python.isort
      # ms-python.python
      # ms-python.vscode-pylance
      ms-toolsai.jupyter
      ms-toolsai.jupyter-keymap
      ms-toolsai.jupyter-renderers
      ms-toolsai.vscode-jupyter-cell-tags
      ms-toolsai.vscode-jupyter-slideshow
      ms-vscode-remote.remote-containers
      ms-vscode-remote.remote-ssh
      ms-vscode-remote.remote-ssh-edit
      ms-vscode-remote.remote-wsl
      ms-vscode.cmake-tools
      ms-vscode.cpptools
      ms-vscode.cpptools-extension-pack
      ms-vscode.hexeditor
      ms-vscode.live-server
      ms-vscode.makefile-tools
      ms-vscode.powershell
      naumovs.color-highlight
      pkief.material-icon-theme
      tamasfe.even-better-toml
      twxs.cmake
      vscodevim.vim
      wmaurer.change-case
      zhuangtongfa.material-theme
    ];
    profiles.default.userSettings = {
      # "workbench.colorTheme" = "Andromeda";
      "workbench.iconTheme" = "material-icon-theme";
      "color-highlight.enable" = false;
      "diffEditor.ignoreTrimWhitespace" = false;
      "editor.bracketPairColorization.enabled" = true;
      # "editor.fontFamily" = "'JetBrainsMono Nerd Font', Consolas, 'Courier New', monospace";
      "editor.fontLigatures" = true;
      # "editor.fontSize" = 11;
      "editor.fontWeight" = "normal";
      "editor.guides.bracketPairs" = "active";
      "editor.renderWhitespace" = "boundary";
      "files.autoSave" = "off";
      "files.insertFinalNewline" = true;
      "telemetry.telemetryLevel" = "off";
      # "terminal.integrated.fontFamily" = "'JetBrainsMono Nerd Font'";
      # "terminal.integrated.fontSize" = 10;
      "terminal.integrated.fontWeight" = "normal";
      "tws.highlightTrailingWhiteSpace" = true;
      "vim.ignorecase" = false;
      "vim.smartRelativeLine" = true;
      "vim.leader" = "\\";
      "vim.normalModeKeyBindings" = [
        {
          "before" = [
            "<leader>"
            "f"
          ];
          "commands" = [ "editor.action.formatDocument" ];
        }
      ];
      "vim.visualModeKeyBindings" = [
        {
          "before" = [ ">" ];
          "commands" = [ "editor.action.indentLines" ];
        }
        {
          "before" = [ "<" ];
          "commands" = [ "editor.action.outdentLines" ];
        }
        {
          "before" = [
            "g"
            "s"
          ];
          "commands" = [
            "editor.action.sortLinesAscending"
            "extension.vim_escape"
          ];
        }
        {
          "before" = [
            "<leader>"
            "f"
          ];
          "commands" = [
            "editor.action.formatSelection"
            "extension.vim_escape"
          ];
        }
      ];
    };
  };
}
