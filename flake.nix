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

      config = {
        allowUnfree = true;
        home = let
          SUDO_USER = builtins.getEnv "SUDO_USER";
          USER = builtins.getEnv "USER";
          HOME = builtins.getEnv "HOME";
        in rec {
          username =
            if !lib.inPureEvalMode
            then
              (
                if SUDO_USER != ""
                then SUDO_USER
                else if USER != ""
                then USER
                else "delacruz"
              )
            else "delacruz";

          name =
            if username == "delacruz"
            then "Hansel De La Cruz"
            else "";

          homeDirectory =
            if !lib.inPureEvalMode
            then
              (
                if SUDO_USER == "" && HOME != ""
                then HOME
                else "/home/${username}"
              )
            else "/home/${username}";
        };
      };

      overlays = [self.overlay];
    };

    lib = nixpkgs.lib.extend (self: super: {
      core = {
        user = (import ./core/user/lib/index.nix) {
          inherit pkgs;

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
      "desktop"
      "minimal"
      "server"
      "work0"
      "work1"
    ];
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
                    user = userPreset;
                  };
                in {
                  inherit system;

                  modules = [
                    {
                      nixpkgs = {
                        inherit (pkgs) config overlays;
                      };
                    }
                    ./preset/system/${systemPreset}.nix
                    home-manager.nixosModules.home-manager
                    {
                      home-manager.useGlobalPkgs = true;
                      home-manager.useUserPackages = true;
                      home-manager.users.${pkgs.config.home.username} = import ./preset/user/${userPreset}.nix;

                      home-manager.sharedModules = [homeage.homeManagerModules.homeage];

                      home-manager.extraSpecialArgs = {
                        inherit preset;
                      };
                    }
                  ];

                  specialArgs = {
                    inherit preset;
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
          )
          userPresets
      )
      otherSystemPresets
    );
  };
}
