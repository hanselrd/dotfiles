{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.user.homeage;
in {
  options = {
    roles.user.homeage = {
      enable = lib.mkEnableOption "roles.user.homeage";
    };
  };

  config = lib.mkIf cfg.enable {};
}
