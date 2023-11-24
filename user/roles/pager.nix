{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.user.pager;
in {
  options = {
    roles.user.pager = {
      enable = lib.mkEnableOption "roles.user.pager";
    };
  };

  config = lib.mkIf cfg.enable {};
}
