{
  config,
  lib,
  pkgs,
  ...
}: {
  home.packages = with pkgs; [
    alejandra
    nixpkgs-fmt
  ];
}