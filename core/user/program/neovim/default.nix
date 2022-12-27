{
  config,
  lib,
  pkgs,
  ...
}: {
  plugins = with pkgs.vimPlugins; [
    cmp-buffer
    cmp-cmdline
    cmp-nvim-lsp
    cmp-path
    cmp-vsnip
    git-blame-nvim
    lualine-lsp-progress
    lualine-nvim
    nvim-base16
    nvim-cmp
    nvim-lspconfig
    nvim-ts-rainbow
    nvim-web-devicons
    pears-nvim
    plenary-nvim
    telescope-fzf-native-nvim
    telescope-nvim
    vim-better-whitespace
    vim-commentary
    vim-eunuch
    vim-fugitive
    vim-gitgutter
    vim-polyglot
    vim-sort-motion
    vim-startify
    vim-textobj-entire
    vim-textobj-function
    vim-vinegar
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
    (pkgs.vimUtils.buildVimPluginFrom2Nix
      {
        name = "vim-textobj-indent";
        version = "unstable";
        src = pkgs.fetchFromGitHub {
          owner = "kana";
          repo = "vim-textobj-indent";
          rev = "deb76867c302f933c8f21753806cbf2d8461b548";
          sha256 = "oFzUPG+IOkbKZ2gU/kduQ3G/LsLDlEjFhRP0BHBE+1Q=";
        };
      })
  ];
  extraPackages = with pkgs; [
    gopls
    jdt-language-server
    nodePackages.purescript-language-server
    nodePackages.pyright
    nodePackages.typescript
    nodePackages.typescript-language-server
    rnix-lsp
    rust-analyzer
    sumneko-lua-language-server
  ];
  viAlias = true;
  vimAlias = true;
  vimdiffAlias = true;
  withNodeJs = true;
  extraConfig = ''
    lua << EOF
      ${builtins.readFile ./config.lua}
    EOF
  '';
}
