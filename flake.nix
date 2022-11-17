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
    system =
      if !lib.inPureEvalMode
      then builtins.currentSystem
      else "x86_64-linux";

    pkgs = import nixpkgs {
      inherit system;

      overlays = [self.overlay];

      config = {
        allowUnfree = true;
        home = rec {
          username =
            if !lib.inPureEvalMode
            then builtins.getEnv "USER"
            else "delacruz";

          homeDirectory =
            if !lib.inPureEvalMode
            then builtins.getEnv "HOME"
            else "/home/${username}";
        };
      };
    };

    lib = nixpkgs.lib.extend (self: super: {
      core = {
        user = (import ./core/user/lib/index.nix) {
          inherit pkgs;

          lib = self;
        };
      };
    });
  in {
    overlay = final: prev: {
      inherit lib;
    };

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
                  };
                in {
                  inherit system;

                  modules = [
                    ./preset/system/${systemPreset}.nix
                    home-manager.nixosModules.home-manager
                    {
                      home-manager.useGlobalPkgs = true;
                      home-manager.useUserPackages = true;
                      home-manager.users.${pkgs.config.home.username} = import ./preset/user/${userPreset}.nix;

                      # TODO: add homeage module and make sure it works
                      # sharedModules = [homeage.homeManagerModules.homeage];

                      # Optionally, use home-manager.extraSpecialArgs to pass
                      # arguments to home.nix
                    }
                  ];

                  specialArgs = {
                    inherit preset;
                  };
                }
              );
            }
          ) ["desktop" "minimal" "server"]
      ) ["nixos"]
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
      ) ["linux-systemd" "linux"]
    );
  };
}
