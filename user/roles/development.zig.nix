{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.user.development.zig;
in {
  options = {
    roles.user.development.zig = {
      enable = lib.mkEnableOption "roles.user.development.zig";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      zigpkgs.master
    ];
  };
}
