{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.user.docker;
in {
  options = {
    roles.user.docker = {
      enable = lib.mkEnableOption "roles.user.docker";
    };
  };

  config = lib.mkIf cfg.enable {};
}
