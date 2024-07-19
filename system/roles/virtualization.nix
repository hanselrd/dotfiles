{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.system.virtualization;
in {
  options = {
    roles.system.virtualization = {
      enable = lib.mkEnableOption "roles.system.virtualization";
    };
  };

  config = lib.mkIf cfg.enable (
    {}
    // lib.optionalAttrs (!lib.profiles.isSystemDarwin) {
      virtualisation.docker = {
        enable = true;
        autoPrune = {
          enable = true;
          dates = "weekly";
          flags = ["--all"];
        };
      };
    }
  );
}
