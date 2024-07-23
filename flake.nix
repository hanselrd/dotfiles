{
  description = "dotfiles";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.darwin.follows = "nix-darwin";
      inputs.home-manager.follows = "home-manager";
    };

    chaotic = {
      url = "github:chaotic-cx/nyx/nyxpkgs-unstable";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
    };

    garuda = {
      url = "gitlab:garuda-linux/garuda-nix-subsystem/stable";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.chaotic-nyx.follows = "chaotic";
      inputs.home-manager.follows = "home-manager";
    };

    gitignore = {
      url = "github:hercules-ci/gitignore.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-colors = {
      url = "github:Misterio77/nix-colors";
      # inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixos-wsl = {
      url = "github:nix-community/NixOS-WSL";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    zig-overlay = {
      url = "github:mitchellh/zig-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    zls = {
      url = "github:zigtools/zls";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.gitignore.follows = "gitignore";
      inputs.zig-overlay.follows = "zig-overlay";
    };
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    agenix,
    chaotic,
    garuda,
    gitignore,
    home-manager,
    nix-colors,
    nix-darwin,
    nixos-wsl,
    rust-overlay,
    zig-overlay,
    zls,
  }: let
    system =
      if !lib.inPureEvalMode
      then builtins.currentSystem
      else "x86_64-linux";

    pkgs = import nixpkgs {
      inherit system;

      config = {
        allowUnfree = true;
        allowUnfreePredicate = _: true;
      };

      overlays = [
        rust-overlay.overlays.default
        zig-overlay.overlays.default
        (final: prev: {
          agenix = agenix.packages.${system}.default;
          dotfiles-scripts = prev.buildGoModule {
            name = "dotfiles-scripts";
            src = gitignore.lib.gitignoreSource ./.;
            vendorHash = "sha256-a7wP8Gt6TQItd24O3I4Oe6hH4ESVMNWoHbWofDhjQTY=";
            subPackages = [
              "scripts/dotfiles-cli"
            ];
            CGO_ENABLED = 0;
          };
        })
      ];
    };

    env = builtins.fromJSON (
      builtins.readFile (
        pkgs.runCommand "dotfiles-cli-environment-json" {}
        "${lib.getExe' pkgs.dotfiles-scripts "dotfiles-cli"} environment > $out"
      )
    );

    lib = nixpkgs.lib;

    mkLib = {profile, ...}:
      nixpkgs.lib.extend (final: prev: {
        vendor = (import ./lib/vendor.nix) {
          inherit inputs pkgs env;
          lib = final;
        };
        common = (import ./lib/common.nix) {
          inherit pkgs env;
          lib = final;
        };
        profiles = (import ./lib/profiles.nix) {
          inherit pkgs env profile;
          lib = final;
        };
      });
  in rec {
    nixosConfigurations = builtins.listToAttrs (
      builtins.map (
        profile: let
          lib = mkLib {inherit profile;};
        in {
          name = profile.name;
          value =
            (
              if !lib.profiles.isSystemGaruda
              then nixpkgs.lib.nixosSystem
              else garuda.lib.garudaSystem
            ) {
              inherit system;

              modules = lib.flatten [
                {
                  nixpkgs = {
                    inherit (pkgs) config overlays;
                  };
                }
                (lib.optional (!lib.profiles.isSystemGaruda) chaotic.nixosModules.default)
                (lib.optional lib.profiles.isSystemWsl nixos-wsl.nixosModules.wsl)
                ./system/roles.nix
                ./system/profiles/${profile.system}.nix
                (lib.optional (!lib.profiles.isSystemGaruda) home-manager.nixosModules.home-manager)
              ];

              specialArgs = {
                inherit inputs lib env profile;
              };
            };
        }
      )
      (env.profiles.nixos ++ env.profiles.garuda ++ env.profiles.wsl)
    );

    darwinConfigurations = builtins.listToAttrs (
      builtins.map (
        profile: let
          lib = mkLib {inherit profile;};
        in {
          name = profile.name;
          value = nix-darwin.lib.darwinSystem {
            inherit system;

            modules = [
              {
                nixpkgs = {
                  inherit (pkgs) config overlays;
                };
              }
              ./system/roles.nix
              ./system/profiles/${profile.system}.nix
              home-manager.darwinModules.home-manager
            ];

            specialArgs = {
              inherit inputs lib env profile;
            };
          };
        }
      )
      env.profiles.darwin
    );

    homeConfigurations = builtins.listToAttrs (
      builtins.map (
        profile: let
          lib = mkLib {inherit profile;};
        in {
          name = profile.name;
          value = home-manager.lib.homeManagerConfiguration {
            inherit pkgs lib;

            modules =
              lib.vendor.home-manager.modules
              ++ (lib.singleton ./user/profiles/${profile.user}.nix);

            extraSpecialArgs = {
              inherit inputs env profile;
            };
          };
        }
      )
      env.profiles.homeManager
    );

    packages = {
      ${system} = rec {
        dotfiles-codegen0 =
          pkgs.writeShellScriptBin "dotfiles-codegen0"
          ''
            ${lib.getExe' pkgs.go "go"} generate ./...
          '';

        dotfiles-codegen1 =
          pkgs.writeShellScriptBin "dotfiles-codegen1"
          ''
            ${lib.getExe dotfiles-codegen0}
            # ${lib.getExe' pkgs.dotfiles-scripts "dotfiles-cli"} codegen hash
            ${lib.getExe' pkgs.dotfiles-scripts "dotfiles-cli"} codegen profiles
            ${lib.getExe' pkgs.dotfiles-scripts "dotfiles-cli"} codegen roles
            ${lib.getExe' pkgs.dotfiles-scripts "dotfiles-cli"} environment > environment.json
            ${lib.getExe' pkgs.dotfiles-scripts "dotfiles-cli"} docker-compose > docker-compose.json
          '';

        dotfiles-update =
          pkgs.writeShellScriptBin "dotfiles-update"
          ''
            nix flake update

            ${lib.getExe' pkgs.go "go"} get -u ./...
            ${lib.getExe' pkgs.go "go"} mod tidy
            ${lib.getExe' pkgs.go "go"} get github.com/dave/jennifer
            ${lib.getExe' pkgs.go "go"} get github.com/dmarkham/enumer
            ${lib.getExe' pkgs.go "go"} get github.com/iancoleman/strcase
          '';
        dotfiles-upgrade = dotfiles-update;

        dotfiles-format =
          pkgs.writeShellScriptBin "dotfiles-format"
          ''
            ${lib.getExe' pkgs.coreutils "echo"} "Formatting *.nix file(s)"
            ${lib.getExe' pkgs.findutils "find"} $PWD -type f ! -path "*/ancestry/*" -name "*.nix" -print -exec ${lib.getExe pkgs.alejandra} -q {} \;

            # ${lib.getExe' pkgs.coreutils "echo"} "Formatting *.sh file(s)"
            # ${lib.getExe' pkgs.findutils "find"} $PWD -type f ! -path "*/ancestry/*" -name "*.sh" -print -exec ${lib.getExe pkgs.shfmt} -w -p -i 2 -sr {} \;

            ${lib.getExe' pkgs.coreutils "echo"} "Formatting *.lua file(s)"
            ${lib.getExe' pkgs.findutils "find"} $PWD -type f ! -path "*/ancestry/*" -name "*.lua" -print -exec ${lib.getExe pkgs.stylua} --indent-type=Spaces --indent-width=2 {} \;

            ${lib.getExe' pkgs.coreutils "echo"} "Formatting *.go file(s)"
            ${lib.getExe' pkgs.findutils "find"} $PWD -type f ! -path "*/ancestry/*" -name "*.go" -print -exec ${lib.getExe' pkgs.gotools "goimports"} -w -local github.com/hanselrd/dotfiles {} \;
            ${lib.getExe' pkgs.findutils "find"} $PWD -type f ! -path "*/ancestry/*" -name "*.go" -print -exec ${lib.getExe pkgs.gofumpt} -w -extra {} \;
            ${lib.getExe' pkgs.findutils "find"} $PWD -type f ! -path "*/ancestry/*" -name "*.go" -print -exec ${lib.getExe' pkgs.golines "golines"} -w -m 100 {} \;
          '';

        dotfiles-all =
          pkgs.writeShellScriptBin "dotfiles-all"
          ''
            ${lib.getExe dotfiles-upgrade}
            ${lib.getExe dotfiles-codegen1}
            ${lib.getExe dotfiles-format}
          '';
      };
    };

    apps = {
      ${system} = {
        dotfiles-cli = {
          type = "app";
          program = lib.getExe' pkgs.dotfiles-scripts "dotfiles-cli";
        };
      };
    };
  };
}
