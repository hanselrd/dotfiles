{ inputs, sharedModulesPath, ... }:
{
  imports = with inputs; [
    home-manager.darwinModules.home-manager
    (sharedModulesPath + "/home-manager-integration")
  ];
}
