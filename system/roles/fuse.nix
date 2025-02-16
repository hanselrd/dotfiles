{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.roles.system.fuse;
in
{
  options = {
    roles.system.fuse = {
      enable = lib.mkEnableOption "roles.system.fuse";
    };
  };

  config = lib.mkIf cfg.enable (
    lib.optionalAttrs (!lib.profiles.isSystemDarwin) {
      programs.fuse.userAllowOther = true;
    }
  );
}
