{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.user.editor;
in {
  options = {
    roles.user.editor = {
      enable = lib.mkEnableOption "roles.user.editor";
    };
  };

  config = lib.mkIf cfg.enable {};
}
