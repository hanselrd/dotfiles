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
      git-blame-nvim
      mini-nvim
      nvim-lspconfig
      nvim-treesitter-context
      nvim-treesitter.withAllGrammars
      plenary-nvim
      rainbow-delimiters-nvim
      telescope-fzf-native-nvim
      telescope-nvim
      vim-abolish
      vim-eunuch
      vim-textobj-entire
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
        inherit (env) name timeFormat;
        inherit (config.home) username;
        inherit (config.xdg) cacheHome;
      }
      |> lib.readFile;
  };
}
