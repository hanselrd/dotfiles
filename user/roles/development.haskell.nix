{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.roles.user.development.haskell;
in
{
  options = {
    roles.user.development.haskell = {
      enable = lib.mkEnableOption "roles.user.development.haskell";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      cabal-install
      stack
    ];
  };
}
