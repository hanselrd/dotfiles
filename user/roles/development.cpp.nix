{
  config,
  lib,
  pkgs,
  env,
  ...
}:
let
  cfg = config.roles.user.development.cpp;
in
{
  options = {
    roles.user.development.cpp = {
      enable = lib.mkEnableOption "roles.user.development.cpp";
    };
  };

  config = lib.mkIf cfg.enable {
    roles.user.gdb.enable = true;
    roles.user.lldb.enable = true;

    home.packages = with pkgs; [
      bear
      ccache
      clang-tools
      cmake
      cmake-format
      emscripten
      gcc
      llvm
      meson
      ninja
    ];

    home.sessionVariables = {
      CCACHE_COMPRESS = "true";
      CCACHE_COMPRESSLEVEL = 6;
      CCACHE_MAXSIZE = "30G";
      CCACHE_SLOPPINESS = "pch_defines,time_macros";
      CMAKE_BUILD_TYPE = "Release";
      CMAKE_COLOR_DIAGNOSTICS = "ON";
      CMAKE_GENERATOR = "Ninja";
      # CMAKE_PREFIX_PATH = "/nix/var/nix/profiles/per-user/${config.home.username}/home-manager/home-path";
      CPM_SOURCE_CACHE = "${env.user.cacheDirectory}/cpm";
    };
  };
}
