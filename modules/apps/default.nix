{ self, ... }: {
  apps = self.lib.eachSystem (
    system:
    let
      pkgs = self.legacyPackages.${system};
      dotfiles = pkgs.haskellPackages.callCabal2nix "dotfiles" self.outPath { };
      dotfilesWithSecrets =
        pkgs.haskellPackages.callCabal2nixWithOptions "dotfiles" self.outPath "-f secrets"
          { };
    in
    {
      builtins = self.lib.mkApp' dotfiles "builtins";
      scripts = self.lib.mkApp' dotfilesWithSecrets "scripts";

      codegen = self.lib.mkApp (
        pkgs.writeShellApplication {
          name = "codegen";
          runtimeInputs = with pkgs; [
            go
            nix
          ];
          text = ''
            nix run .#scripts
            go generate ./...
          '';
        }
      );

      update = self.lib.mkApp (
        pkgs.writeShellApplication {
          name = "update";
          runtimeInputs = with pkgs; [
            go
            nix
          ];
          text = ''
            nix flake update
            go get -u ./...
            go mod tidy
            go get github.com/dave/jennifer
            go get github.com/dmarkham/enumer
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

      canary = self.lib.mkApp (self.lib.buildGoBin "canary" { inherit pkgs; });

      eject = self.lib.mkApp' dotfiles "eject";
      update-hashes = self.lib.mkApp' dotfiles "update-hashes";
    }
  );
}
