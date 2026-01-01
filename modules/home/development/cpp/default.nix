{ pkgs, ... }:
{
  home.packages = with pkgs; [
    bear
    clang-tools
    emscripten
    gcc
    llvm
    meson
  ];
}
