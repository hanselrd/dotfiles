{
  config,
  lib,
  pkgs,
  ...
}:
{
  programs.eza = {
    enable = true;
    enableBashIntegration = config.programs.bash.enable;
    enableZshIntegration = config.programs.zsh.enable;
    extraOptions = [
      "--group"
      "--group-directories-first"
      "--octal-permissions"
      "--time-style=iso"
      # "--total-size"
    ];
  };

  home.shellAliases = {
    lta = "lt -a";
    llt = "ll --tree";
    llta = "llt -a";
  };
}
