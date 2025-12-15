{
  config,
  lib,
  pkgs,
  ...
}:
{
  home.packages = with pkgs; [
    lua
    luarocks
    stylua
  ];
}
