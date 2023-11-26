{
  description = "dotfiles";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    homeage = {
      url = "github:jordanisaacs/homeage";
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
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    home-manager,
    homeage,
    nix-colors,
    nix-darwin,
  }: let
    system =
      if !lib.trivial.inPureEvalMode
      then builtins.currentSystem
      else "x86_64-linux";

    pkgs = import nixpkgs {
      inherit system;

      config = {
        allowUnfree = true;
        # colorScheme = nix-colors.colorSchemes.chalk;
        colorScheme = env.theme;
        home = {
          username = env.user.username;
          homeDirectory = env.user.homeDirectory;
        };
      };
    };

    scripts = pkgs.buildGoModule {
      name = "dotfiles-scripts";
      src = ./.;
      vendorHash = "sha256-WttOOKf1ZepZbAu8E+HHv0OEyLDz7k+BRdtuaficrvA=";
      subPackages = [
        "scripts/dotfiles-cli"
      ];
      CGO_ENABLED = 0;
    };

    env = builtins.fromJSON (
      builtins.readFile (
        pkgs.runCommand "dotfiles-cli-environment-json" {}
        "${lib.getExe' scripts "dotfiles-cli"} environment > $out"
      )
    );

    lib = nixpkgs.lib.extend (self: super: {
      vendor =
        (import ./lib/vendor.nix)
        (inputs
          // {
            inherit pkgs system env;
            lib = self;
          });
      core = {
        user = (import ./lib/user.nix) {
          inherit pkgs env;
          lib = self;
        };
        common = (import ./lib/common.nix) {
          inherit pkgs env;
          lib = self;
        };
      };
    });
  in rec {
    nixosConfigurations = builtins.listToAttrs (
      builtins.map
      (
        profile: {
          name = profile.name;
          value = nixpkgs.lib.nixosSystem {
            inherit system;

            modules = [
              {
                nixpkgs = {
                  inherit (pkgs) config;
                };
              }
              ./system/roles.nix
              ./system/profiles/${profile.system}.nix
              home-manager.nixosModules.home-manager
              {
                home-manager.useGlobalPkgs = true;
                home-manager.useUserPackages = true;
                home-manager.users.${pkgs.config.home.username} = import ./user/profiles/${profile.user}.nix;

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
        profile: {
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
            ${lib.getExe' scripts "dotfiles-cli"} environment > environment.json
            ${lib.getExe' scripts "dotfiles-cli"} dockerCompose > docker-compose.json
            ${lib.getExe' scripts "dotfiles-cli"} template
          '';

        dotfiles-scripts = scripts;

        dotfiles-update =
          pkgs.writeShellScriptBin "dotfiles-update"
          ''
            nix flake update

            ${lib.getExe' pkgs.go "go"} get -u ./...
            ${lib.getExe' pkgs.go "go"} mod tidy
            ${lib.getExe' pkgs.go "go"} get github.com/dave/jennifer

            pushd user/roles/neovim/nodePackages
            ${lib.getExe' pkgs.node2nix "node2nix"} -i <(echo "[\"emmet-ls\"]")
            popd
          '';
        dotfiles-upgrade = dotfiles-update;

        dotfiles-format =
          pkgs.writeShellScriptBin "dotfiles-format"
          ''
            echo "Formatting *.nix file(s)"
            find $PWD -type f ! -path "*/ancestry/*" -name "*.nix" -print -exec ${lib.getExe pkgs.alejandra} -q {} \;

            echo "Formatting *.ncl file(s)"
            find $PWD -type f ! -path "*/ancestry/*" -name "*.ncl" -print -exec ${lib.getExe' pkgs.topiary "topiary"} -l nickel -f {} --in-place \;

            echo "Formatting *.go file(s)"
            find $PWD -type f ! -path "*/ancestry/*" -name "*.go" -print -exec ${lib.getExe' pkgs.go "gofmt"} -w {} \;

            echo "Formatting *.sh file(s)"
            find $PWD -type f ! -path "*/ancestry/*" -name "*.sh" -print -exec ${lib.getExe pkgs.shfmt} -w -p -i 2 -sr {} \;

            echo "Formatting *.lua file(s)"
            find $PWD -type f ! -path "*/ancestry/*" -name "*.lua" -print -exec ${lib.getExe pkgs.stylua} --indent-type=Spaces --indent-width=2 {} \;
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
          program = lib.getExe' packages.${system}.dotfiles-scripts "dotfiles-cli";
        };
      };
    };
  };
}
