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
    (homeModulesPath + "/development/cpp")
    (lib.x.decryptSecretModule env.identity (secretHomeModulesPath + "/work"))
  ];

  home.stateVersion = "26.05";
}
