{ homeModulesPath, ... }: {
  imports = [
    (homeModulesPath + "/common")
    (homeModulesPath + "/bash-to-zsh-override")
    (homeModulesPath + "/development/cpp")
    (homeModulesPath + "/development/java")
    (homeModulesPath + "/development/python")
  ];

  home.stateVersion = "26.05";
}
