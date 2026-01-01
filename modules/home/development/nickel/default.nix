{ pkgs, ... }:
{
  home.packages = with pkgs; [
    nickel
    topiary
  ];
}
