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

  stylix.targets.fzf.enable = true;
}
