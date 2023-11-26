{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.user.development.nix;
in {
  options = {
    roles.user.development.nix = {
      enable = lib.mkEnableOption "roles.user.development.nix";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      alejandra
      nixpkgs-fmt
    ];
  };
}
