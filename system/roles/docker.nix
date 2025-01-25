{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.roles.system.docker;
in
{
  options = {
    roles.system.docker = {
      enable = lib.mkEnableOption "roles.system.docker";
    };
  };

  config = lib.mkIf cfg.enable (
    lib.optionalAttrs (!lib.profiles.isSystemDarwin) {
      virtualisation.docker = {
        enable = true;
        autoPrune = {
          enable = true;
          dates = "weekly";
          flags = [ "--all" ];
        };
      };
    }
  );
}
