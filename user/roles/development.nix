{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.user.development;
in {
  options = {
    roles.user.development = {
      enable = lib.mkEnableOption "roles.user.development";
    };
  };

  config = lib.mkIf cfg.enable {};
}
