{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.user.ranger;
in {
  options = {
    roles.user.ranger = {
      enable = lib.mkEnableOption "roles.user.ranger";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.ranger.enable = true;
  };
}
