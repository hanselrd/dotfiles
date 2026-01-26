{ pkgs, ... }:
{
  home.packages = with pkgs; [
    nickel
    nls
    topiary
  ];
}
