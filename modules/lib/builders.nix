{
  self,
  inputs,
  lib,
  ...
}:
let
  inherit (inputs) nixpkgs home-manager nix-darwin;
in
{
  lib = rec {
    mkNixosConfiguration =
      module:
      nixpkgs.lib.nixosSystem {
        modules = [
          # nixpkgs.nixosModules.readOnlyPkgs
          ({ config, ... }: { nixpkgs.pkgs = self.legacyPackages.${config.nixpkgs.hostPlatform.system}; })
          module
        ];
      };

    mkDarwinConfiguration =
      module:
      nix-darwin.lib.darwinSystem {
        modules = [
          ({ config, ... }: { nixpkgs.pkgs = self.legacyPackages.${config.nixpkgs.hostPlatform.system}; })
          module
        ];
      };

    mkHomeConfiguration =
      system: module:
      home-manager.lib.homeManagerConfiguration {
        pkgs = self.legacyPackages.${system};
        modules = [ module ];
      };

    mkApp' = drv: exeName: {
      type = "app";
      program = lib.getExe' drv exeName;
      meta.description = exeName;
    };

    mkApp = drv: mkApp' drv drv.meta.mainProgram;

    buildGoBin =
      name:
      { pkgs }:
      pkgs.buildGoModule {
        name = "dotfiles-go-bin-${name}";
        src = self.outPath;
        vendorHash = "sha256-3EzYAzCxUOrMDZN5cRq/b466vus0Dp5cpJ0Vct8ypgM=";
        subPackages = [ "cmd/${name}" ];
        goSum = self.outPath + "/go.sum";
        ldflags = [
          "-s -w -linkmode=external"
          "-X 'github.com/hanselrd/dotfiles/internal/build.Version=${lib.version}'"
          "-X 'github.com/hanselrd/dotfiles/internal/build.PureEvalMode=${builtins.toString lib.inPureEvalMode}'"
          "-X 'github.com/hanselrd/dotfiles/internal/build.RootDir=${builtins.toString self.outPath}'"
          "-X 'github.com/hanselrd/dotfiles/internal/build.Dirty=${builtins.toString (!(self ? shortRev))}'"
        ];
        meta.mainProgram = name;
      };
  };
}
