{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.roles.user.development.nickel;
in
{
  options = {
    roles.user.development.nickel = {
      enable = lib.mkEnableOption "roles.user.development.nickel";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      nickel
      topiary
    ];
  };
}
