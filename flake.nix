{
  description = "dotfiles";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    gitignore = {
      url = "github:hercules-ci/gitignore.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    homeage = {
      url = "github:jordanisaacs/homeage/pull/43/head";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-colors = {
      url = "github:misterio77/nix-colors";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-darwin = {
      url = "github:LnL7/nix-darwin";
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
      inputs.zig-overlay.follows = "zig-overlay";
    };
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    gitignore,
    home-manager,
    homeage,
    nix-colors,
    nix-darwin,
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

      overlays = [
        rust-overlay.overlays.default
        zig-overlay.overlays.default
        (final: prev: {
          zls = zls.packages.${system}.default;
          zon2nix = prev.zon2nix.overrideAttrs (old: {
            version = "master";
            src = prev.fetchFromGitHub {
              owner = "nix-community";
              repo = "zon2nix";
              rev = "pull/8/head";
              hash = "sha256-0pkNLXJF83Ezk5eSgnMR7kU5XXFpkIqTM7KKZpe0VTc=";
            };
          });
        })
        (final: prev: {
          dotfiles-scripts = prev.buildGoModule {
            name = "dotfiles-scripts";
            src = gitignore.lib.gitignoreSource ./.;
            vendorHash = "sha256-MwM4e25kriIKU8OmVtRUViLh1u9lUvtAlxwDWiMd9DA=";
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
        vendor =
          (import ./lib/vendor.nix)
          (inputs
            // {
              inherit pkgs env;
              lib = final;
            });
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
      builtins.map
      (
        profile: let
          lib = mkLib {inherit profile;};
        in {
          name = profile.name;
          value = nixpkgs.lib.nixosSystem {
            inherit system;

            modules = [
              {
                nixpkgs = {
                  inherit (pkgs) overlays;
                };
              }
              ./system/roles.nix
              ./system/profiles/${profile.system}.nix
              home-manager.nixosModules.home-manager
              {
                home-manager.useGlobalPkgs = true;
                home-manager.useUserPackages = true;
                home-manager.users.${env.user.username} = import ./user/profiles/${profile.user}.nix;

                home-manager.sharedModules = [
                  homeage.homeManagerModules.homeage
                  nix-colors.homeManagerModules.default
                  ./user/roles.nix
                ];

                home-manager.extraSpecialArgs =
                  inputs
                  // {
                    inherit env profile;
                  };
              }
            ];

            specialArgs =
              inputs
              // {
                inherit lib env profile;
              };
          };
        }
      )
      env.profiles.nixos
    );

    homeConfigurations = builtins.listToAttrs (
      builtins.map
      (
        profile: let
          lib = mkLib {inherit profile;};
        in {
          name = profile.name;
          value = home-manager.lib.homeManagerConfiguration {
            inherit pkgs lib;

            modules = [
              homeage.homeManagerModules.homeage
              nix-colors.homeManagerModules.default
              ./user/roles.nix
              ./user/profiles/${profile.user}.nix
            ];

            extraSpecialArgs =
              inputs
              // {
                inherit pkgs env profile;
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
            ${lib.getExe' pkgs.dotfiles-scripts "dotfiles-cli"} environment > environment.json
            ${lib.getExe' pkgs.dotfiles-scripts "dotfiles-cli"} dockerCompose > docker-compose.json
            ${lib.getExe' pkgs.dotfiles-scripts "dotfiles-cli"} template
          '';

        dotfiles-update =
          pkgs.writeShellScriptBin "dotfiles-update"
          ''
            nix flake update

            ${lib.getExe' pkgs.go "go"} get -u ./...
            ${lib.getExe' pkgs.go "go"} mod tidy
            ${lib.getExe' pkgs.go "go"} get github.com/dave/jennifer

            pushd user/roles/neovim/nodePackages
            ${lib.getExe' pkgs.node2nix "node2nix"} -i <(${lib.getExe' pkgs.coreutils "echo"} "[\"emmet-ls\"]")
            popd
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
            ${lib.getExe' pkgs.findutils "find"} $PWD -type f ! -path "*/ancestry/*" -name "*.go" -print -exec ${lib.getExe' pkgs.go "gofmt"} -w {} \;

            ${lib.getExe' pkgs.coreutils "echo"} "Formatting *.rs file(s)"
            ${lib.getExe' pkgs.findutils "find"} $PWD -type f ! -path "*/ancestry/*" -name "*.rs" -print -exec ${lib.getExe' pkgs.rust-bin.nightly.latest.default "rustfmt"} {} \;

            ${lib.getExe' pkgs.coreutils "echo"} "Formatting *.zig file(s)"
            ${lib.getExe' pkgs.findutils "find"} $PWD -type f ! -path "*/ancestry/*" -name "*.zig" -print -exec ${lib.getExe' pkgs.zigpkgs.master "zig"} fmt {} \;
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
