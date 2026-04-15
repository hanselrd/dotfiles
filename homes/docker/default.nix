{ lib, homeModulesPath, ... }:
{
  imports = [
    (homeModulesPath + "/common")
    (homeModulesPath + "/bash-to-zsh-override")
  ];

  nix.settings.sandbox = lib.mkForce false;

  home.stateVersion = "26.05";
}
