{ self, inputs, ... }:
let
  inherit (inputs) nixpkgs import-tree nix-unit;
in
{
  checks = self.lib.eachSystem (system: {
    testing =
      self.legacyPackages.${system}.runCommand "tests"
        { nativeBuildInputs = [ nix-unit.packages.${system}.default ]; }
        ''
          export HOME="$(realpath .)"
          nix-unit --eval-store "$HOME" \
            --extra-experimental-features flakes \
            --override-input nixpkgs ${nixpkgs.outPath} \
            --override-input import-tree ${import-tree.outPath} \
            --flake ${self.outPath}#tests
          touch $out
        '';
  });
}
