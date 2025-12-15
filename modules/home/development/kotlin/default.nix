{
  config,
  lib,
  pkgs,
  ...
}:
{
  home.packages = with pkgs; [
    gradle
    kotlin
  ];
}
