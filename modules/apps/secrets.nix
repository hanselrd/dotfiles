{ self, lib, ... }:
let
  rules = import (self.outPath + "/secrets/secrets.nix");
in
{
  apps = self.lib.eachSystem (
    system:
    let
      pkgs = self.legacyPackages.${system};
    in
    {
      checksum-secrets = self.lib.mkApp (
        pkgs.writeShellApplication {
          name = "checksum-secrets";
          runtimeInputs = with pkgs; [ findutils ];
          text = ''
            find secrets -type f ! -path "*sha256sums.txt" -printf "%P\0" | sort -z | env -C secrets xargs -0 sha256sum > secrets/sha256sums.txt
          '';
        }
      );

      encrypt-secrets = self.lib.mkApp (
        pkgs.writeShellApplication {
          name = "encrypt-secrets";
          runtimeInputs = with pkgs; [
            age
            gnugrep
            nix
          ];
          text = ''
            ${lib.concatStringsSep "\n" (
              lib.mapAttrsToList (
                name: value:
                let
                  encrypted = name;
                  cleartext = lib.removeSuffix ".age" name;
                in
                "test -s secrets/${cleartext} && { ! grep -s '${cleartext}$' secrets/sha256sums.txt | env -C secrets sha256sum -c -; } && age ${
                  lib.concatMapStringsSep " " (x: "-r '${x}'") value.publicKeys
                } ${
                  if lib.hasAttr "armor" value && value.armor then "--armor" else ""
                } -o secrets/${encrypted} secrets/${cleartext} || true"
              ) rules
            )}
            nix run .#checksum-secrets
          '';
        }
      );

      decrypt-secrets = self.lib.mkApp (
        pkgs.writeShellApplication {
          name = "decrypt-secrets";
          runtimeInputs = with pkgs; [
            agenix
            gnugrep
            nix
          ];
          text = ''
            ${lib.concatMapStringsSep "\n" (
              x:
              let
                encrypted = x;
                cleartext = lib.removeSuffix ".age" x;
              in
              "{ ! grep -s '${encrypted}$' secrets/sha256sums.txt | env -C secrets sha256sum -c -; } && { env -C secrets agenix -d ${encrypted} 2>/dev/null || true; } > secrets/${cleartext}"
            ) (lib.attrNames rules)}
            nix run .#checksum-secrets
          '';
        }
      );
    }
  );
}
