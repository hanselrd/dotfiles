{
  config,
  lib,
  pkgs,
  env,
  ...
}:
let
  cfg = config.roles.user.xdg;
in
{
  options = {
    roles.user.xdg = {
      enable = lib.mkEnableOption "roles.user.xdg";
    };
  };

  config = lib.mkIf cfg.enable {
    xdg.cacheHome = env.user.cacheDirectory;
    xdg.configHome = env.user.configDirectory;
    xdg.dataHome = env.user.dataDirectory;
    xdg.stateHome = env.user.stateDirectory;

    xdg.userDirs.createDirectories = lib.profiles.isUserStandard || lib.profiles.isUserFull;
  };
}
