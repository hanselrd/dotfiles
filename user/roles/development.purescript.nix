{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.roles.user.development.purescript;
in
{
  options = {
    roles.user.development.purescript = {
      enable = lib.mkEnableOption "roles.user.development.purescript";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      purescript
      # spago
    ];
  };
}
