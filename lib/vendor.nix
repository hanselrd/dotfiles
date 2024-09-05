{
  inputs,
  lib,
  pkgs,
  env,
  ...
}: let
  inherit (inputs) chaotic nix-colors;
in {
  nix-colors = nix-colors.lib;
  nix-colors-contrib = nix-colors.lib.contrib {inherit pkgs;};
  nix-colors-custom = (import ./vendor/nix-colors-custom.nix) {
    inherit pkgs lib;
  };

  home-manager.modules = lib.flatten [
    (lib.optional (!lib.profiles.isSystemGaruda) chaotic.homeManagerModules.default)
    nix-colors.homeManagerModules.default
    ../user/roles.nix
  ];
}
