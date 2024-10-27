{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.user.eza;
in {
  options = {
    roles.user.eza = {
      enable = lib.mkEnableOption "roles.user.eza";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.eza = {
      enable = true;
      enableBashIntegration = true;
      enableZshIntegration = true;
      extraOptions = [
        "--group"
        "--group-directories-first"
        "--octal-permissions"
        "--time-style=iso"
        # "--total-size"
      ];
    };

    home.shellAliases = {
      lta = "lt -a";
    };
  };
}
