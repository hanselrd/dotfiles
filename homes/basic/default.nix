{ homeModulesPath, ... }: {
  imports = [
    (homeModulesPath + "/common")
    (homeModulesPath + "/development")
    (homeModulesPath + "/packages/standard")
  ];

  home.stateVersion = "26.05";
}
