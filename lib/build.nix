{
  inputs,
  outputs,
  overlays,
  legacyPackages,
  lib,
  rootPath,
  sharedModulesPath,
  nixosModulesPath,
  darwinModulesPath,
  homeModulesPath,
  secretsPath,
  secretSharedModulesPath,
  secretNixosModulesPath,
  secretDarwinModulesPath,
  secretHomeModulesPath,
  ...
}:
rec {
  mkEnv =
    {
      username ? "delacruz",
      name ? "Hansel De La Cruz",
      email ? "18725263+hanselrd@users.noreply.github.com",
      emailSecret ? null,
      homeDirectory ? "/home/${username}",
      identity ? "${homeDirectory}/.ssh/id_ed25519",
      theme ? "chalk",
      homeName ? "basic",
      hostName ? "nohost0",
      system ? "x86_64-linux",
      timeZone ? "America/New_York",
      timeFormat ? "<%a>%-d-%b-%y <%Z>T%H:%M",
      goTimeFormat ? "<Mon>2-Jan-06 <MST>T15:04",
      nixRoot ? null,
    }@args:
    assert lib.assertMsg (
      (!(args ? email) && !(args ? emailSecret))
      || (!(args ? email) && (args ? emailSecret))
      || ((args ? email) && !(args ? emailSecret))
    ) "email and emailSecret are mutually exclusive";
    args
    // {
      inherit
        username
        name
        # email
        # emailSecret
        homeDirectory
        identity
        theme
        homeName
        hostName
        system
        timeZone
        timeFormat
        goTimeFormat
        nixRoot
        ;
      email = if emailSecret != null then lib.x.readSecret identity emailSecret else email;
    };

  mkNixosConfiguration =
    env:
    inputs.nixpkgs.lib.nixosSystem {
      modules = [ (rootPath + "/hosts/${env.hostName}") ];
      specialArgs = {
        inherit
          inputs
          outputs
          overlays
          lib
          rootPath
          sharedModulesPath
          nixosModulesPath
          homeModulesPath
          secretsPath
          secretSharedModulesPath
          secretNixosModulesPath
          secretHomeModulesPath
          env
          ;
      };
    };

  mkDarwinConfiguration =
    env:
    inputs.nix-darwin.lib.darwinSystem {
      modules = [ (rootPath + "/hosts/${env.hostName}") ];
      specialArgs = {
        inherit
          inputs
          outputs
          overlays
          lib
          rootPath
          sharedModulesPath
          darwinModulesPath
          homeModulesPath
          secretsPath
          secretSharedModulesPath
          secretDarwinModulesPath
          secretHomeModulesPath
          env
          ;
      };
    };

  mkHomeConfiguration =
    env:
    inputs.home-manager.lib.homeManagerConfiguration {
      pkgs = legacyPackages.${env.system};
      modules = [ (rootPath + "/homes/${env.homeName}") ];
      extraSpecialArgs = {
        inherit
          inputs
          outputs
          overlays
          lib
          rootPath
          sharedModulesPath
          homeModulesPath
          secretsPath
          secretSharedModulesPath
          secretHomeModulesPath
          env
          ;
      };
    };

  mkApp' = drv: exeName: {
    type = "app";
    program = lib.getExe' drv exeName;
  };

  mkApp = drv: mkApp' drv drv.meta.mainProgram;
}
