{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.user.terminal;
in {
  options = {
    roles.user.terminal = {
      enable = lib.mkEnableOption "roles.user.terminal";
    };
  };

  config = lib.mkIf cfg.enable {
    roles.user.alacritty.enable = true;

    home.sessionVariables = {
      TERMINAL = "alacritty";
    };
  };
}
