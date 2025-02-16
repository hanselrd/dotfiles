{
  config,
  lib,
  pkgs,
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
    roles.user.ccache.enable = true;
    roles.user.cmake.enable = true;
    roles.user.gdb.enable = true;
    roles.user.lldb.enable = true;

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
