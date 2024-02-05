{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.system.x11;
in {
  options = {
    roles.system.x11 = {
      enable = lib.mkEnableOption "roles.system.x11";
    };
  };

  config = lib.mkIf cfg.enable {
    services.xserver = {
      xkb = {
        layout = "us";
        variant = "";
      };
    };
  };
}
