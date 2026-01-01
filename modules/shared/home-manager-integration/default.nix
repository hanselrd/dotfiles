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
  home-manager.backupFileExtension = "bkp.${lib.x.getRandomString 5}";
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
