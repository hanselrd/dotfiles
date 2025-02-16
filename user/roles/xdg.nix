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
    xdg = {
      enable = true;
      cacheHome = env.user.cacheDirectory;
      configHome = env.user.configDirectory;
      dataHome = env.user.dataDirectory;
      stateHome = env.user.stateDirectory;
    };

    xdg.userDirs = lib.mkIf (lib.profiles.isUserStandard || lib.profiles.isUserFull) {
      enable = true;
      createDirectories = true;
    };
  };
}
