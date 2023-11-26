{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.user.theme;
in {
  options = {
    roles.user.theme = {
      enable = lib.mkEnableOption "roles.user.theme";
    };
  };

  config = lib.mkIf cfg.enable {
    colorScheme = pkgs.config.colorScheme;
  };
}
