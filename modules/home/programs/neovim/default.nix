{
  config,
  lib,
  pkgs,
  env,
  ...
}:
{
  programs.neovim = {
    enable = true;
    plugins = with pkgs.vimPlugins; [
      mini-nvim
      nvim-lspconfig
      nvim-treesitter-context
      plenary-nvim
      rainbow-delimiters-nvim
      telescope-fzf-native-nvim
      telescope-nvim
      vim-abolish
      vim-eunuch
      vim-textobj-entire
      (pkgs.stable.vimPlugins.nvim-treesitter.withPlugins (_: pkgs.stable.tree-sitter.allGrammars))
    ];
    extraPackages = with pkgs; [
      emmet-ls
      vscode-json-languageserver
    ];
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
    withNodeJs = true;
    initLua =
      pkgs.replaceVars ./config.lua {
        inherit (env) name;
        inherit (config.home) username;
        inherit (config.xdg) cacheHome;
      }
      |> lib.readFile;
  };
}
