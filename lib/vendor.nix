{
  inputs,
  lib,
  pkgs,
  env,
  ...
}: let
  inherit (inputs) nix-colors;
in {
  nix-colors = nix-colors.lib;
  nix-colors-contrib = nix-colors.lib.contrib {inherit pkgs;};
  nix-colors-custom = (import ./vendor/nix-colors-custom.nix) {
    inherit pkgs lib;
  };
}
