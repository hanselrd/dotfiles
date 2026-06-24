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
      (pkgs.vimUtils.buildVimPlugin rec{
        pname = "amber-vim";
        version = "0cd0acda4e3d168ac7a3d1a8fa6a361e37bbb6bc"; # github:amber-lang/amber-vim/main
        src = pkgs.fetchFromGitHub {
          owner = "amber-lang";
          repo = "amber-vim";
          rev = version;
          hash = "sha256-eQV2YUs7brdSC1g4tsm4pRlXQaqZl8d4BJEi3xBR3kI=";
        };
      })
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
