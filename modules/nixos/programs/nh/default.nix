{
  config,
  lib,
  pkgs,
  env,
  ...
}:
{
  programs.nh = {
    enable = true;
    flake = "${env.homeDirectory}/.dotfiles";
  };

  environment.sessionVariables = {
    NH_SHOW_ACTIVATION_LOGS = 1;
  };
}
