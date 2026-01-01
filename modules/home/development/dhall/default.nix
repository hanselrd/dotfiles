{ pkgs, ... }:
{
  home.packages = with pkgs; [
    dhall
    dhall-bash
    dhall-json
    dhall-nix
  ];
}
