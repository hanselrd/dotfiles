{ darwinModulesPath, ... }:
{
  imports = [ (darwinModulesPath + "/common") ];

  system.stateVersion = 6;
}
