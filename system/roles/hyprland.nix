{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.system.hyprland;
in {
  options = {
    roles.system.hyprland = {
      enable = lib.mkEnableOption "roles.system.hyprland";
    };
  };

  config = lib.mkIf cfg.enable (
    lib.optionalAttrs (!lib.profiles.isSystemDarwin) {
      programs.hyprland.enable = true;
    }
  );
}
