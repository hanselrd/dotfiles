{ pkgs, ... }:
{
  home.packages = with pkgs; [
    alejandra
    nixfmt-rfc-style
    nixpkgs-fmt
  ];
}
