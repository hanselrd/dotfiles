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

    nix.optimise = {
      automatic = true;
      dates = ["weekly"];
    };

    nix.gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };
  };
}
