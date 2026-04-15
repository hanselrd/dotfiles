{ homeModulesPath, ... }:
{
  imports = [
    (homeModulesPath + "/common")
    (homeModulesPath + "/bash-to-zsh-override")
  ];

  home.stateVersion = "26.05";
}
