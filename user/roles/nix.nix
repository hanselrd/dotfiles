{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.user.nix;
in {
  options = {
    roles.user.nix = {
      enable = lib.mkEnableOption "roles.user.nix";
    };
  };

  config = lib.mkIf cfg.enable {};
}
