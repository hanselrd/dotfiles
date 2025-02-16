{
  config,
  lib,
  pkgs,
  env,
  ...
}:
let
  cfg = config.roles.user.cmake;
in
{
  options = {
    roles.user.cmake = {
      enable = lib.mkEnableOption "roles.user.cmake";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      cmake
      cmake-format
      ninja
    ];

    home.sessionVariables = {
      CMAKE_BUILD_TYPE = "Release";
      CMAKE_COLOR_DIAGNOSTICS = "ON";
      CMAKE_GENERATOR = "Ninja";
      # CMAKE_PREFIX_PATH = "/nix/var/nix/profiles/per-user/${env.user.username}/home-manager/home-path";
      CPM_SOURCE_CACHE = "${env.user.cacheDirectory}/cpm";
    };
  };
}
