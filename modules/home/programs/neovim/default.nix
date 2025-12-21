{
  config,
  lib,
  pkgs,
  ...
}:
{
  programs.neovim = {
    enable = true;
    plugins = with pkgs.vimPlugins; [
      mini-nvim
      nvim-lspconfig
      rainbow-delimiters-nvim
      telescope-fzf-native-nvim
      telescope-nvim
      vim-abolish
      vim-eunuch
      vim-textobj-entire
      (nvim-treesitter.withPlugins (_: pkgs.stable.tree-sitter.allGrammars))
    ];
    extraPackages = with pkgs; [
      clang-tools
      elixir-ls
      emmet-ls
      gopls
      jdt-language-server
      kotlin-language-server
      lua-language-server
      nixd
      nls
      nodePackages.typescript
      nodePackages.typescript-language-server
      pyright
      rust-analyzer
      vscode-json-languageserver
      zls
    ];
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
    withNodeJs = true;
    extraLuaConfig = pkgs.replaceVars ./config.lua { inherit (config.xdg) cacheHome; } |> lib.readFile;
  };
}
