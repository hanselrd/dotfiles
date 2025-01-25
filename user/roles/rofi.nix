{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.roles.user.rofi;
in
{
  options = {
    roles.user.rofi = {
      enable = lib.mkEnableOption "roles.user.rofi";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.rofi = {
      enable = true;
      # TODO: font/theme
    };
  };
}
