{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.user.browser;
in {
  options = {
    roles.user.browser = {
      enable = lib.mkEnableOption "roles.user.browser";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      brave
    ];

    home.sessionVariables = {
      BROWSER = "brave";
    };
  };
}
