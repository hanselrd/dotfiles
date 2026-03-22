{ homeModulesPath, ... }:
{
  imports = [
    (homeModulesPath + "/common")
    (homeModulesPath + "/development")
    (homeModulesPath + "/packages/standard")
  ];

  home.stateVersion = "25.11";
}
