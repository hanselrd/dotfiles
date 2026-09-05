{
  modules.home.cpp-development = { pkgs, ... }: {
    home.packages = with pkgs; [
      bear
      clang-tools
      emscripten
      gcc
      llvm
      meson
    ];
  };
}
