{
  config,
  lib,
  pkgs,
  ...
}: {
  programs.java = lib.core.user.mkProgram "java" {};

  home.packages = with pkgs; [
    maven
  ];
}
