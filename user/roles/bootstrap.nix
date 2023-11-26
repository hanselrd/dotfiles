{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.user.bootstrap;
in {
  options = {
    roles.user.bootstrap = {
      enable = lib.mkEnableOption "roles.user.bootstrap";
    };
  };

  config = lib.mkIf cfg.enable {
    home.username = pkgs.config.home.username;
    home.homeDirectory = pkgs.config.home.homeDirectory;

    programs.home-manager.enable = true;

    home.enableNixpkgsReleaseCheck = false;

    home.stateVersion = "22.05";
  };
}
