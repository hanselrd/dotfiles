{
  config,
  lib,
  pkgs,
  env,
  ...
}:
{
  home.packages = with pkgs; [
    cmake
    cmake-format
    ninja
  ];

  home.sessionVariables = {
    CMAKE_BUILD_TYPE = "Release";
    CMAKE_COLOR_DIAGNOSTICS = "ON";
    CMAKE_GENERATOR = "Ninja";
    CPM_SOURCE_CACHE = "${config.xdg.cacheHome}/cpm";
  };
}
