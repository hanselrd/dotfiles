{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.user.shell;
in {
  options = {
    roles.user.shell = {
      enable = lib.mkEnableOption "roles.user.shell";
    };
  };

  config = lib.mkIf cfg.enable {};
}
