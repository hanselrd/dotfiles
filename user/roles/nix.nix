{
  nixpkgs,
  config,
  lib,
  pkgs,
  env,
  ...
}: let
  cfg = config.roles.user.nix;
in {
  options = {
    roles.user.nix = {
      enable = lib.mkEnableOption "roles.user.nix";
    };
  };

  config = lib.mkIf cfg.enable {
    nix.registry.nixpkgs.flake = nixpkgs;

    nix.package = lib.mkDefault pkgs.nix;

    nix.settings = {
      experimental-features = ["nix-command" "flakes"];
      sandbox = env.roles.user.nix.sandbox;
      show-trace = true;
    };

    nixpkgs.config.allowUnfree = true;

    programs.nix-index.enable = true;
  };
}
