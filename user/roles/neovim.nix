{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.user.neovim;
in {
  options = {
    roles.user.neovim = {
      enable = lib.mkEnableOption "roles.user.neovim";
    };
  };

  config = lib.mkIf cfg.enable {
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
        {
          plugin = lib.vendor.nix-colors-contrib.vimThemeFromScheme {scheme = config.colorScheme;};
          config = ''
            lua << EOF
              vim.cmd [[ colorscheme nix-${config.colorScheme.slug} ]]
            EOF
          '';
        }
        (nvim-treesitter.withPlugins (plugins: pkgs.tree-sitter.allGrammars))
        # (
        #   nvim-treesitter.withPlugins (
        #     plugins:
        #       with plugins; [
        #         tree-sitter-nix
        #       ]
        #   )
        # )
        (pkgs.vimUtils.buildVimPlugin
          rec {
            name = "vim-textobj-indent";
            version = "master";
            src = pkgs.fetchFromGitHub {
              owner = "kana";
              repo = name;
              rev = "deb76867c302f933c8f21753806cbf2d8461b548";
              hash = "sha256-oFzUPG+IOkbKZ2gU/kduQ3G/LsLDlEjFhRP0BHBE+1Q=";
            };
          })
      ];
      extraPackages = with pkgs; let
        otherNodePackages = callPackage ./neovim/nodePackages {};
      in [
        # elixir-ls
        # nixd
        clang-tools
        gopls
        jdt-language-server
        kotlin-language-server
        lua-language-server
        nls
        nodePackages.purescript-language-server
        nodePackages.pyright
        nodePackages.typescript
        nodePackages.typescript-language-server
        otherNodePackages.emmet-ls
        rust-analyzer
        zls
      ];
      viAlias = true;
      vimAlias = true;
      vimdiffAlias = true;
      withNodeJs = true;
      extraConfig = ''
        lua << EOF
          ${builtins.readFile ./neovim/config.lua}
        EOF
      '';
    };
  };
}
