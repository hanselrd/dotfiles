{ homeModulesPath, ... }:
{
  imports = [
    (homeModulesPath + "/common")
    (homeModulesPath + "/bash-to-zsh-override")
  ];

  nix.settings.sandbox = false;

  home.stateVersion = "22.05";
}
