{ homeModulesPath, ... }:
{
  imports = [
    (homeModulesPath + "/common")
    (homeModulesPath + "/development")
    (homeModulesPath + "/packages/standard")
  ];

  home.stateVersion = "22.05";
}
