{ self, ... }: {
  apps = self.lib.eachSystem (
    system:
    let
      pkgs = self.legacyPackages.${system};
    in
    {
      builtins = self.lib.mkApp' self.packages.${system}.dotfiles "builtins";

      codegen-ocaml = self.lib.mkApp' self.packages.${system}.dotfiles-with-secrets "codegen";

      codegen = self.lib.mkApp (
        pkgs.writeShellApplication {
          name = "codegen";
          runtimeInputs = with pkgs; [
            dune
            nix
          ];
          text = ''
            dune build @opam --auto-promote || true
            nix run .#codegen-ocaml
          '';
        }
      );

      update-ocaml = self.lib.mkApp' self.packages.${system}.dotfiles "update";

      update = self.lib.mkApp (
        pkgs.writeShellApplication {
          name = "update";
          runtimeInputs = with pkgs; [ nix ];
          text = ''
            nix flake update
            # nix run .#update-ocaml
          '';
        }
      );

      all = self.lib.mkApp (
        pkgs.writeShellApplication {
          name = "all";
          runtimeInputs = with pkgs; [ nix ];
          text = ''
            nix run .#codegen
            nix fmt
            nix run .#update
          '';
        }
      );

      # canary = self.lib.mkApp (self.lib.buildGoBin "canary" { inherit pkgs; });

      eject = self.lib.mkApp' self.packages.${system}.dotfiles "eject";
    }
  );
}
