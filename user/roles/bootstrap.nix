{
  config,
  lib,
  pkgs,
  env,
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
    home.username = env.user.username;
    home.homeDirectory = env.user.homeDirectory;

    programs.home-manager.enable = true;

    home.enableNixpkgsReleaseCheck = false;

    home.stateVersion = "22.05";
  };
}
