{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.user.development.kotlin;
in {
  options = {
    roles.user.development.kotlin = {
      enable = lib.mkEnableOption "roles.user.development.kotlin";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      gradle
      kotlin
    ];
  };
}
