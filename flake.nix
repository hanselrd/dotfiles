{
  description = "dotfiles";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs-stable.url = "github:NixOS/nixpkgs/nixos-25.11";

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
      inputs.zig-overlay.follows = "zig-overlay";
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
            emailSecret = ./secrets/work_email.age;
            homeName = "work0";
          }
        );
        work1 = lib.x.mkHomeConfiguration (
          lib.x.mkEnv {
            username = "hansel.delacruz";
            emailSecret = ./secrets/work_email.age;
            homeName = "work1";
          }
        );
      };

      apps = eachSystem (
        system:
        let
          pkgs = legacyPackages.${system};
        in
        {
          codegen = lib.x.mkApp (
            pkgs.writeShellApplication {
              name = "codegen";
              runtimeInputs = with pkgs; [ go ];
              text = ''
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
                nix fmt
                nix run .#update
              '';
            }
          );

          eject = lib.x.mkApp (
            pkgs.writeShellApplication {
              name = "eject";
              runtimeInputs = with pkgs; [
                coreutils
                cpio
                findutils
                gnused
                nix
              ];
              text = ''
                set -x
                home=''${1:-basic}
                hash_length=5
                out_dir=$HOME/.nix/x/$(
                  < /dev/urandom tr -dc 'a-z0-9' | head -c "$hash_length"
                  echo
                )
                path0=$(nix build --no-link --print-out-paths ".#homeConfigurations.$home.activationPackage" --impure)
                path1=$(readlink -f "$HOME/.nix-profile")
                deps=$(nix-store -qR "$path0" "$path1")
                sed_string="s@/nix/store/.{$((''${#out_dir} - 10))}@$out_dir/@g"
                echo "$deps" | xargs -I {} -P 0 sh -c "find {} | cpio -ov | sed -E '$sed_string' | cpio -idmv"
                chmod -R u+w "$out_dir"
                path0_new=$(echo "$path0" | sed -E "$sed_string")
                path1_new=$(echo "$path1" | sed -E "$sed_string")
                cp -a "$path0_new/home-files/." "$HOME/"
                ln -snfF "$path1_new" "$HOME/.nix-profile"
              '';
            }
          );

          update-hashes = lib.x.mkApp (lib.x.buildGoBin "update-hashes" { inherit pkgs; });
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
              nh
              nix
              nix-plugins
              pkgs.home-manager
            ];
            NH_FLAKE = ./.;
            shellHook = ''
              . ${./bootstrap/nix-config.sh}
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
