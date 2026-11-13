{ self, inputs, ... }:
let
  inherit (inputs) opam-nix;
in
{
  packages = self.lib.eachSystem (
    system:
    let
      pkgs = self.legacyPackages.${system};
    in
    rec {
      dotfiles =
        (opam-nix.lib.${system}.buildDuneProject { inherit pkgs; } "dotfiles" self.outPath {
          ocaml-config = "*";
        }).dotfiles;

      dotfiles-with-secrets = dotfiles.overrideAttrs { DOTFILES_SECRETS = true; };
    }
  );
}
