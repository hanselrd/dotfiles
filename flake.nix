{
  description = "A very basic flake";

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
  };

  outputs = {
    self,
    nixpkgs,
    home-manager,
    homeage,
  }: let
    pkgs = import nixpkgs {
      inherit system;

      config = {
        allowUnfree = true;
        home = rec {
          username = "delacruz";
          homeDirectory = "/home/${username}";
        };
      };
    };

    system = "x86_64-linux";
  in {
    nixosConfigurations = builtins.listToAttrs (
      map
      (
        systemPreset: {
          name = systemPreset;
          value = nixpkgs.lib.nixosSystem (
            let
              preset = {
                system = systemPreset;
              };
            in {
              inherit system;

              modules = [./preset/system/${systemPreset}.nix];

              extraSpecialArgs = {
                inherit preset;
              };
            }
          );
        }
      )
      ["nixos"]
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
                  inherit pkgs;

                  lib = pkgs.lib.extend (self: super: {
                    ext = (import ./core/user/lib/index.nix) {
                      inherit pkgs preset;

                      lib = self;
                    };
                  });

                  modules = [
                    homeage.homeManagerModules.homeage
                    ./preset/user/${userPreset}.nix
                  ];

                  extraSpecialArgs = {
                    inherit preset;
                  };
                }
              );
            }
          ) ["desktop" "minimal" "server"]
      ) ["linux-systemd" "linux" "nixos"]
    );
  };
}
