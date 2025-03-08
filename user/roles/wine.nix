{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.roles.user.wine;
in
{
  options = {
    roles.user.wine = {
      enable = lib.mkEnableOption "roles.user.wine";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      # wineWowPackages.stagingFull
      # wineWowPackages.waylandFull
      wineWowPackages.stableFull
      winetricks
    ];
  };
}
