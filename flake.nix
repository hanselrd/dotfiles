{
  description = "dotfiles";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs-stable.url = "github:NixOS/nixpkgs/nixos-26.05";

    amber = {
      url = "github:amber-lang/amber";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.rust-overlay.follows = "rust-overlay";
    };

    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.darwin.follows = "nix-darwin";
      inputs.home-manager.follows = "home-manager";
    };

    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    impermanence = {
      url = "github:nix-community/impermanence";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
    };

    nix-darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixos-hardware = {
      url = "github:NixOS/nixos-hardware";
    };

    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    stylix = {
      url = "github:nix-community/stylix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    zig-overlay = {
      url = "github:mitchellh/zig-overlay";
    };

    zls = {
      url = "github:zigtools/zls";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      home-manager,
      treefmt-nix,
      ...
    }@inputs:
    let
      inherit (self) outputs;

      rootPath = ./.;
      sharedModulesPath = rootPath + "/modules/shared";
      nixosModulesPath = rootPath + "/modules/nixos";
      darwinModulesPath = rootPath + "/modules/darwin";
      homeModulesPath = rootPath + "/modules/home";
      secretsPath = rootPath + "/secrets";
      secretSharedModulesPath = secretsPath + "/modules/shared";
      secretNixosModulesPath = secretsPath + "/modules/nixos";
      secretDarwinModulesPath = secretsPath + "/modules/darwin";
      secretHomeModulesPath = secretsPath + "/modules/home";

      lib = nixpkgs.lib.extend (
        final: _prev:
        (import ./lib {
          inherit
            inputs
            outputs
            overlays
            legacyPackages
            rootPath
            sharedModulesPath
            nixosModulesPath
            darwinModulesPath
            homeModulesPath
            secretsPath
            secretSharedModulesPath
            secretNixosModulesPath
            secretDarwinModulesPath
            secretHomeModulesPath
            ;
          lib = final;
        })
        // home-manager.lib
      );

      overlays = import ./overlays { inherit inputs outputs lib; };

      eachSystem = lib.genAttrs [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];

      legacyPackages = eachSystem (
        system:
        import nixpkgs {
          inherit system overlays;
          config = {
            allowUnfree = true;
            allowUnfreePredicate = _: true;
          };
        }
      );

      treefmtEval = eachSystem (
        system: treefmt-nix.lib.evalModule legacyPackages.${system} ./treefmt.nix
      );
    in
    {
      nixosConfigurations = {
        infinity = lib.x.mkNixosConfiguration (lib.x.mkEnv { hostName = "infinity"; });
      };

      darwinConfigurations = {
        macbook = lib.x.mkDarwinConfiguration (
          lib.x.mkEnv {
            homeDirectory = "/Users/delacruz";
            hostName = "macbook";
            system = "aarch64-darwin";
          }
        );
      };

      homeConfigurations = {
        docker = lib.x.mkHomeConfiguration (
          lib.x.mkEnv {
            username = "root";
            homeDirectory = "/root";
            homeName = "docker";
          }
        );
        basic = lib.x.mkHomeConfiguration (lib.x.mkEnv { homeName = "basic"; });
        work0 = lib.x.mkHomeConfiguration (
          lib.x.mkEnv {
            email = lib.fileContents ./secrets/work-email;
            homeName = "work0";
          }
        );
        work1 = lib.x.mkHomeConfiguration (
          lib.x.mkEnv {
            username = "hansel.delacruz";
            email = lib.fileContents ./secrets/work-email;
            homeName = "work1";
          }
        );
      };

      apps = eachSystem (
        system:
        let
          rules = import ./secrets/secrets.nix;

          pkgs = legacyPackages.${system};

          dotfiles = pkgs.haskellPackages.callCabal2nix "dotfiles" ./. { };
          dotfilesWithSecrets = pkgs.haskellPackages.callCabal2nixWithOptions "dotfiles" ./. "-f secrets" { };
        in
        {
          builtins = lib.x.mkApp' dotfiles "builtins";
          scripts = lib.x.mkApp' dotfilesWithSecrets "scripts";

          encrypt-secrets = lib.x.mkApp (
            pkgs.writeShellApplication {
              name = "encrypt-secrets";
              runtimeInputs = with pkgs; [ age ];
              text = ''
                ${lib.concatStringsSep "\n" (
                  lib.mapAttrsToList (
                    n: v:
                    "test -s secrets/${lib.removeSuffix ".age" n} && age ${
                      lib.concatMapStringsSep " " (x: "-r '${x}'") v.publicKeys
                    } ${
                      if lib.hasAttr "armor" v && v.armor then "--armor" else ""
                    } -o secrets/${n} secrets/${lib.removeSuffix ".age" n} && sha256sum secrets/${n} || true"
                  ) rules
                )}
              '';
            }
          );

          decrypt-secrets = lib.x.mkApp (
            pkgs.writeShellApplication {
              name = "decrypt-secrets";
              runtimeInputs = with pkgs; [ agenix ];
              text = ''
                ${lib.concatMapStringsSep "\n" (x: ''
                  { env -C secrets agenix -d ${x} 2>/dev/null || true; } > secrets/${lib.removeSuffix ".age" x}
                  sha256sum secrets/${lib.removeSuffix ".age" x}
                '') (lib.attrNames rules)}
              '';
            }
          );

          codegen = lib.x.mkApp (
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

          update = lib.x.mkApp (
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

          all = lib.x.mkApp (
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

          canary = lib.x.mkApp (lib.x.buildGoBin "canary" { inherit pkgs; });

          eject = lib.x.mkApp' dotfiles "eject";
          update-hashes = lib.x.mkApp' dotfiles "update-hashes";
        }
      );

      devShells = eachSystem (
        system:
        let
          pkgs = legacyPackages.${system};
        in
        {
          default = pkgs.mkShellNoCC {
            packages = with pkgs; [
              age
              coreutils
              gnused
              jq
              nh
              nix
              nix-plugins
              pkgs.home-manager
            ];
            NH_FLAKE = ./.;
            shellHook = ''
              . ${./scripts/nix-config.sh}
              export NIX_CONFIG=$(
                cat << EOF
              $NIX_CONFIG
              plugin-files = ${pkgs.nix-plugins}/lib/nix/plugins
              extra-builtins-file = ${./lib/builtins.nix}
              EOF
              )
              nix --version
            '';
          };
        }
      );

      formatter = eachSystem (system: treefmtEval.${system}.config.build.wrapper);

      checks = eachSystem (system: {
        formatting = treefmtEval.${system}.config.build.check self;
      });
    };
}
