{ config, ... }:
{
  programs.nh = {
    enable = true;
    flake = "${config.home.homeDirectory}/.dotfiles";
  };

  home.sessionVariables = {
    NH_SHOW_ACTIVATION_LOGS = 1;
  };
}
