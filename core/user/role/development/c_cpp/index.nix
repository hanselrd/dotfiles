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
    CMAKE_GENERATOR = "Ninja";
    CMAKE_BUILD_TYPE = "Release";
    CPM_SOURCE_CACHE = "~/.cache/CPM";
    CCACHE_COMPRESS = "true";
    CCACHE_SLOPPINESS = "pch_defines,time_macros";
  };
}
