{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.roles.user.fastfetch;
in
{
  options = {
    roles.user.fastfetch = {
      enable = lib.mkEnableOption "roles.user.fastfetch";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.fastfetch = {
      enable = true;
      # TODO: settings
    };
  };
}
