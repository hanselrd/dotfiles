{
  config,
  lib,
  pkgs,
  ...
}:
{
  programs.fzf = {
    enable = true;
    tmux.enableShellIntegration = true;
  };
}
