{
  nix-colors,
  lib,
  pkgs,
  env,
  ...
}: {
  nix-colors = nix-colors.lib;
  nix-colors-contrib = nix-colors.lib.contrib {inherit pkgs;};
  nix-colors-custom = (import ./vendor/nix-colors-custom.nix) {
    inherit pkgs lib;
  };
}
