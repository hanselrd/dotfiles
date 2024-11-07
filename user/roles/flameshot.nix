{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.user.flameshot;
in {
  options = {
    roles.user.flameshot = {
      enable = lib.mkEnableOption "roles.user.flameshot";
    };
  };

  config = lib.mkIf cfg.enable {
    services.flameshot.enable = true;
  };
}
