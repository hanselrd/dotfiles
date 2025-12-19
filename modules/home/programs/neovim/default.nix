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
      close-buffers-vim
      cmp-buffer
      cmp-cmdline
      cmp-nvim-lsp
      cmp-path
      cmp-vsnip
      git-blame-nvim
      lualine-lsp-progress
      lualine-nvim
      nvim-cmp
      nvim-colorizer-lua
      nvim-lspconfig
      nvim-surround
      nvim-treesitter-textobjects
      nvim-web-devicons
      oil-nvim
      pears-nvim
      plenary-nvim
      rainbow-delimiters-nvim
      tagbar
      telescope-fzf-native-nvim
      telescope-nvim
      treesj
      vim-abolish
      vim-better-whitespace
      vim-commentary
      vim-easymotion
      vim-eunuch
      vim-fugitive
      vim-gitgutter
      vim-nickel
      vim-polyglot
      vim-sort-motion
      vim-startify
      vim-textobj-entire
      vim-textobj-function
      vim-unimpaired
      vim-vsnip
      (nvim-treesitter.withPlugins (plugins: pkgs.tree-sitter.allGrammars))
      # (
      #   nvim-treesitter.withPlugins (
      #     plugins:
      #       with plugins; [
      #         tree-sitter-nix
      #       ]
      #   )
      # )
      (pkgs.vimUtils.buildVimPlugin rec {
        name = "vim-textobj-indent";
        version = "master";
        src = pkgs.fetchFromGitHub {
          owner = "kana";
          repo = name;
          rev = version;
          hash = "sha256-oFzUPG+IOkbKZ2gU/kduQ3G/LsLDlEjFhRP0BHBE+1Q=";
        };
      })
    ];
    extraPackages = with pkgs; [
      # elixir-ls
      clang-tools
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
