{
  config,
  lib,
  pkgs,
  ...
}: {
  home.packages = with pkgs; [
    openjdk
    maven
  ];
}
