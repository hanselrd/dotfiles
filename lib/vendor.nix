{
  nickel-nix,
  nix-colors,
  lib,
  pkgs,
  system,
  env,
  ...
}: {
  nickel-nix = nickel-nix.lib.${system};
  nix-colors = nix-colors.lib;
  nix-colors-contrib = nix-colors.lib.contrib {inherit pkgs;};
  nix-colors-custom = (import ../core/vendor/lib/nix-colors-custom.nix) {
    inherit pkgs lib;
  };
}
