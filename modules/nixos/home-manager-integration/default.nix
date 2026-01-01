{ inputs, sharedModulesPath, ... }:
{
  imports = with inputs; [
    home-manager.nixosModules.home-manager
    (sharedModulesPath + "/home-manager-integration")
  ];
}
