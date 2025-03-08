{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.roles.user.bottles;
in
{
  options = {
    roles.user.bottles = {
      enable = lib.mkEnableOption "roles.user.bottles";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      bottles
    ];
  };
}
