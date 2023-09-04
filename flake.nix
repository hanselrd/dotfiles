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

    nickel-nix = {
      url = "github:nickel-lang/nickel-nix";
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

  nixConfig = {
    extra-substituters = ["https://nickel-nix.cachix.org"];
    extra-trusted-public-keys = ["nickel-nix.cachix.org-1:/Ziozgt3g0CfGwGS795wyjRa9ArE89s3tbz31S6xxFM="];
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    home-manager,
    homeage,
    nickel-nix,
    nix-colors,
    nix-darwin,
  }: let
    env = lib.vendor.nickel-nix.importNcl ./. "environment.ncl" {};

    system =
      if !lib.trivial.inPureEvalMode
      then builtins.currentSystem
      else env._system;

    pkgs = import nixpkgs {
      inherit system;

      config = {
        allowUnfree = true;
        colorScheme = nix-colors.colorSchemes.chalk;
        # colorScheme = (import ./core/user/theme/grayscale.nix).colorScheme;
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
  in {
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

    darwinConfigurations = builtins.listToAttrs (
      builtins.concatMap
      (
        systemPreset:
          map
          (
            userPreset: {
              name = "${systemPreset}-${userPreset}";
              value = nix-darwin.lib.darwinSystem (
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
      darwinSystemPresets
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
  };
}
