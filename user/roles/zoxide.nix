{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.user.zoxide;
in {
  options = {
    roles.user.zoxide = {
      enable = lib.mkEnableOption "roles.user.zoxide";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.zoxide = {
      enable = true;
      enableBashIntegration = true;
      enableZshIntegration = true;
      options = ["--cmd cd"];
    };
  };
}
