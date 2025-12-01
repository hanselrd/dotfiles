{
  config,
  lib,
  pkgs,
  env,
  ...
}:
let
  cfg = config.roles.user.nh;
in
{
  options = {
    roles.user.nh = {
      enable = lib.mkEnableOption "roles.user.nh";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.nh = {
      enable = true;
      flake = "${env.user.homeDirectory}/.dotfiles";
    };

    home.sessionVariables = {
      NH_SHOW_ACTIVATION_LOGS = 1;
    };
  };
}
