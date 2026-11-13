{
  modules.home.cpp-development = { pkgs, ... }: {
    home.packages = with pkgs; [
      bear
      clang-tools
      compiledb
      emscripten
      gcc
      llvm
      meson
    ];
  };
}
