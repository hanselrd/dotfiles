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

    organist = {
      url = "github:nickel-lang/organist";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  nixConfig = {
    extra-substituters = ["https://organist.cachix.org"];
    extra-trusted-public-keys = ["organist.cachix.org-1:GB9gOx3rbGl7YEh6DwOscD1+E/Gc5ZCnzqwObNH2Faw="];
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    home-manager,
    homeage,
    nix-colors,
    nix-darwin,
    organist,
  }: let
    env = lib.vendor.organist.importNcl ./. "environment.ncl" {} {};

    system =
      if !lib.trivial.inPureEvalMode
      then builtins.currentSystem
      else "x86_64-linux";

    pkgs = import nixpkgs {
      inherit system;

      config = {
        allowUnfree = true;
        colorScheme = nix-colors.colorSchemes.chalk;
        # colorScheme =
        #   (lib.vendor.organist.importNcl
        #     ./. "lib/themes/${env.theme}.ncl" {} {})
        #   .colorScheme;
        home = {
          username = env.user.username;
          homeDirectory = env.user.homeDirectory;
        };
      };
    };

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

    nixosSystemPresets = ["nixos"];
    darwinSystemPresets = ["macos"];
    otherSystemPresets = [
      "linux-systemd"
      "linux"
    ];
    userPresets = [
      "full"
      "minimal"
      "standard"
      "base"
    ];
  in rec {
    nixosConfigurations = builtins.listToAttrs (
      builtins.concatMap
      (
        systemPreset:
          map
          (
            userPreset: {
              name = "${systemPreset}-${userPreset}";
              value = nixpkgs.lib.nixosSystem (
                let
                  preset = {
                    system = systemPreset;
                    user = userPreset;
                  };
                in {
                  inherit system;

                  modules = [
                    {
                      nixpkgs = {
                        inherit (pkgs) config;
                      };
                    }
                    ./preset/system/${systemPreset}.nix
                    home-manager.nixosModules.home-manager
                    {
                      home-manager.useGlobalPkgs = true;
                      home-manager.useUserPackages = true;
                      home-manager.users.${pkgs.config.home.username} = import ./preset/user/${userPreset}.nix;

                      home-manager.sharedModules = [
                        homeage.homeManagerModules.homeage
                        nix-colors.homeManagerModules.default
                      ];

                      home-manager.extraSpecialArgs =
                        inputs
                        // {
                          inherit env preset;
                        };
                    }
                  ];

                  specialArgs =
                    inputs
                    // {
                      inherit lib env preset;
                    };
                }
              );
            }
          )
          userPresets
      )
      nixosSystemPresets
    );

    homeConfigurations = builtins.listToAttrs (
      builtins.concatMap
      (
        systemPreset:
          map
          (
            userPreset: {
              name = "${systemPreset}-${userPreset}";
              value = home-manager.lib.homeManagerConfiguration (
                let
                  preset = {
                    system = systemPreset;
                    user = userPreset;
                  };
                in {
                  inherit pkgs lib;

                  modules = [
                    homeage.homeManagerModules.homeage
                    nix-colors.homeManagerModules.default
                    ./preset/user/${userPreset}.nix
                  ];

                  extraSpecialArgs =
                    inputs
                    // {
                      inherit env preset;
                    };
                }
              );
            }
          )
          userPresets
      )
      otherSystemPresets
    );

    packages = {
      ${system} = {
        dotfiles-scripts = pkgs.buildGoModule rec {
          name = "dotfiles-scripts";
          src = ./.;
          vendorHash = "sha256-uVwT/XgwgDWiQQgY3Df+EgVreyaTweAHPlXcFyJQb7A=";
          subPackages = [
            "scripts/home-manager"
          ];
        };

        dotfiles-update =
          pkgs.writeShellScriptBin "dotfiles-update"
          ''
            nix flake update

            ${lib.getExe' pkgs.go "go"} get -u ./...

            pushd core/user/program/neovim/nodePackages
            ${lib.getExe' pkgs.node2nix "node2nix"} -i <(echo "[\"emmet-ls\"]")
            popd
          '';

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
          '';
      };
    };

    apps = {
      ${system} = {
        dotfiles-home-manager = {
          type = "app";
          program = lib.getExe' packages.${system}.dotfiles-scripts "home-manager";
        };
      };
    };
  };
}
