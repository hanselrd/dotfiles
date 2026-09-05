{ self, ... }: {
  devShells = self.lib.eachSystem (
    system:
    let
      pkgs = self.legacyPackages.${system};
    in
    {
      default = pkgs.mkShellNoCC {
        packages = with pkgs; [
          age
          coreutils
          gnused
          home-manager
          jq
          nh
          nix
          nix-plugins
        ];
        NH_FLAKE = self.outPath;
        shellHook = ''
          . ${self.outPath + "/scripts/nix-config.sh"}
          export NIX_CONFIG=$(
            cat << EOF
          $NIX_CONFIG
          plugin-files = ${pkgs.nix-plugins}/lib/nix/plugins
          extra-builtins-file = ${self.outPath + "/modules/lib/_builtins.nix"}
          EOF
          )
          nix --version
        '';
      };
    }
  );
}
