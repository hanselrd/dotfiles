{
  config,
  lib,
  pkgs,
  ...
}:
{
  programs.zoxide = {
    enable = true;
    enableBashIntegration = config.programs.bash.enable;
    enableZshIntegration = config.programs.zsh.enable;
    options = [ "--cmd cd" ];
  };
}
