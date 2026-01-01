{ pkgs, ... }:
{
  home.packages = with pkgs; [ zigpkgs.master ];
}
