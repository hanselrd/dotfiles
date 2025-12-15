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
      nixpkgs-stable,
      agenix,
      disko,
      home-manager,
      impermanence,
      nix-darwin,
      nixos-hardware,
      rust-overlay,
      stylix,
      treefmt-nix,
      zig-overlay,
      zls,
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
        final: prev:
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

      # darwinConfigurations = { };

      homeConfigurations = {
        basic = lib.x.mkHomeConfiguration (lib.x.mkEnv { homeName = "basic"; });
        work0 = lib.x.mkHomeConfiguration (
          lib.x.mkEnv {
            emailSecret = ./secrets/work_email.age;
            homeName = "work0";
            nixRoot = "/home/delacruz/.nix";
          }
        );
        work1 = lib.x.mkHomeConfiguration (
          lib.x.mkEnv rec {
            username = "hansel.delacruz";
            emailSecret = ./secrets/work_email.age;
            homeName = "work1";
            nixRoot = "/home/${username}/.nix";
          }
        );
        work2 = lib.x.mkHomeConfiguration (
          lib.x.mkEnv rec {
            username = "hansel.delacruz";
            emailSecret = ./secrets/work_email.age;
            homeName = "work2";
            nixRoot = "/home/${username}/.nix";
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
            pkgs.writeShellScriptBin "codegen" ''
              ${lib.getExe' pkgs.go "go"} generate ./...
            ''
          );

          update = lib.x.mkApp (
            pkgs.writeShellScriptBin "update" ''
              ${lib.getExe pkgs.nix} flake update
              ${lib.getExe' pkgs.go "go"} get -u ./...
              ${lib.getExe' pkgs.go "go"} mod tidy
              ${lib.getExe' pkgs.go "go"} get github.com/dave/jennifer
              ${lib.getExe' pkgs.go "go"} get github.com/dmarkham/enumer
            ''
          );

          all = lib.x.mkApp (
            pkgs.writeShellScriptBin "all" ''
              ${lib.getExe pkgs.nix} fmt
              ${lib.getExe pkgs.nix} run .#update
            ''
          );

          cli = lib.x.mkApp (lib.x.buildGoBin "dotfiles-cli" { inherit pkgs; });
        }
      );

      formatter = eachSystem (system: treefmtEval.${system}.config.build.wrapper);

      checks = eachSystem (system: {
        formatting = treefmtEval.${system}.config.build.check self;
      });
    };
}
