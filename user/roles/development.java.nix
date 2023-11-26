{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.user.development.java;
in {
  options = {
    roles.user.development.java = {
      enable = lib.mkEnableOption "roles.user.development.java";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.java.enable = true;

    home.packages = with pkgs; [
      maven
    ];
  };
}
