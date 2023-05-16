{
  config,
  lib,
  pkgs,
  ...
}: {
  defaultKeymap = "emacs";
  enableAutosuggestions = true;
  enableCompletion = false;
  enableSyntaxHighlighting = true;
  history = {
    expireDuplicatesFirst = true;
  };
  initExtra = ''
    sh ${lib.vendor.nix-colors-contrib.shellThemeFromScheme {scheme = config.colorScheme;}}
  '';
  profileExtra = ''
    if [ -e ${config.home.homeDirectory}/.nix-profile/etc/profile.d/nix.sh ]; then . ${config.home.homeDirectory}/.nix-profile/etc/profile.d/nix.sh; fi
  '';
}
