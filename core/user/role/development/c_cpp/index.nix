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
}
