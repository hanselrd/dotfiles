{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.user.btop;
in {
  options = {
    roles.user.btop = {
      enable = lib.mkEnableOption "roles.user.btop";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.btop.enable = true;
  };
}
