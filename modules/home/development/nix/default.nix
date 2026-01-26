{ pkgs, ... }:
{
  home.packages = with pkgs; [
    alejandra
    nil
    nixfmt
    nixpkgs-fmt
  ];
}
