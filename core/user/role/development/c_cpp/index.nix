{
  config,
  lib,
  pkgs,
  ...
}: {
  home.packages = with pkgs; [
    ccache
    clang-tools
    cmake
    emscripten
    gcc
    gdb
    lldb
    llvm
    meson
    ninja
  ];

  home.sessionVariables = {
    CMAKE_PREFIX_PATH = "/nix/var/nix/profiles/per-user/${config.home.username}/home-manager/home-path";
    CMAKE_GENERATOR = "Ninja";
    CMAKE_BUILD_TYPE = "Release";
    CPM_SOURCE_CACHE = "${config.home.homeDirectory}/.cache/CPM";
    CCACHE_COMPRESS = "true";
    CCACHE_COMPRESSLEVEL = "6";
    CCACHE_MAXSIZE = "30G";
    CCACHE_SLOPPINESS = "pch_defines,time_macros";
  };
}
