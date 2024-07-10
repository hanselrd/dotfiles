{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.system.wsl;
in {
  options = {
    roles.system.wsl = {
      enable = lib.mkEnableOption "roles.system.wsl";
    };
  };

  config = lib.mkIf cfg.enable {
    wsl = {
      enable = true;
      defaultUser = env.user.name;
      # docker-desktop.enable = true;
      # startMenuLaunchers = true;
    };
  };
}
