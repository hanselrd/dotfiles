{
  inputs,
  outputs,
  overlays,
  lib,
  rootPath,
  sharedModulesPath,
  homeModulesPath,
  secretsPath,
  secretSharedModulesPath,
  secretHomeModulesPath,
  env,
  ...
}:
{
  imports = with inputs; [ home-manager.nixosModules.home-manager ];

  home-manager.backupFileExtension = "bkp.${builtins.toString builtins.currentTime}";
  # home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = true;
  home-manager.users.${env.username} = import (rootPath + "/homes/${env.homeName}");

  home-manager.extraSpecialArgs = {
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

  stylix.homeManagerIntegration.autoImport = false;
  stylix.homeManagerIntegration.followSystem = false;
}
