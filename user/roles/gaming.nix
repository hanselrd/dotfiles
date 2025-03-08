{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.roles.user.gaming;
in
{
  options = {
    roles.user.gaming = {
      enable = lib.mkEnableOption "roles.user.gaming";
    };
  };

  config = lib.mkIf cfg.enable {
    roles.user.bottles.enable = true;
    roles.user.wine.enable = true;

    home.packages = with pkgs; [
      heroic
      lutris
      protonup-qt
    ];
  };
}
