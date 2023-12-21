{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.system.nix;
in {
  options = {
    roles.system.nix = {
      enable = lib.mkEnableOption "roles.system.nix";
    };
  };

  config = lib.mkIf cfg.enable {
    nix.settings = {
      auto-optimise-store = true;
      experimental-features = ["nix-command" "flakes"];
      show-trace = true;
      trusted-users = ["root" "@wheel"];
    };

    nixpkgs.config.allowUnfree = true;
  };
}
