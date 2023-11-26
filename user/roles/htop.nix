{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.user.htop;
in {
  options = {
    roles.user.htop = {
      enable = lib.mkEnableOption "roles.user.htop";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.htop = {
      enable = true;
      settings = {
        shadow_other_users = 1;
      };
    };
  };
}
