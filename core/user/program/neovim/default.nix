{
  config,
  lib,
  pkgs,
  ...
}: {
  # coc.enable = true;
  plugins = with pkgs.vimPlugins; [
    vim-nix
    vim-sensible
    yankring
    {
      plugin = vim-startify;
      config = "let g:startify_change_to_vcs_root = 0";
    }
  ];
  viAlias = true;
  vimAlias = true;
  vimdiffAlias = true;
  withNodeJs = true;
}
