{
  lib,
  homeModulesPath,
  secretHomeModulesPath,
  env,
  ...
}:
{
  imports = [
    (homeModulesPath + "/common")
    (homeModulesPath + "/bash-to-zsh-override")
    (lib.x.decryptSecretModule env.identity (secretHomeModulesPath + "/work"))
  ];

  home.stateVersion = "25.11";
}
