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

  config = lib.mkIf cfg.enable (
    lib.recursiveUpdate
    {
      nix.settings = {
        auto-optimise-store = !lib.profiles.isSystemDarwin;
        experimental-features = ["nix-command" "flakes"];
        show-trace = true;
        trusted-users = ["root" "@wheel"];
      };

      nix.optimise = {
        automatic = true;
        dates = lib.mkIf (!lib.profiles.isSystemDarwin) ["weekly"];
      };

      nix.gc = {
        automatic = true;
        dates = lib.mkIf (!lib.profiles.isSystemDarwin) "weekly";
        options = "--delete-older-than 7d";
      };
    }
    (lib.optionalAttrs lib.profiles.isSystemDarwin {
      nix.optimise.interval = [
        {
          Hour = 3;
          Minute = 15;
          Weekday = 7;
        }
      ];

      nix.gc.interval = [
        {
          Hour = 3;
          Minute = 15;
          Weekday = 7;
        }
      ];
    })
  );
}
