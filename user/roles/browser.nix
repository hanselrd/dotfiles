{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.roles.user.browser;
in
{
  options = {
    roles.user.browser = {
      enable = lib.mkEnableOption "roles.user.browser";
    };
  };

  config = lib.mkIf cfg.enable {
    roles.user.brave.enable = true;

    home.sessionVariables = {
      BROWSER = "brave";
    };
  };
}
