{
  config,
  lib,
  pkgs,
  env,
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
    ${
      if env.shellTheme
      then ''
        sh ${lib.vendor.nix-colors-contrib.shellThemeFromScheme {scheme = config.colorScheme;}}
      ''
      else ""
    }

    ${
      if env.homeageSecrets
      then ''
        if [ -e ${config.home.homeDirectory}/.secrets/rts.sh ]; then
          . ${config.home.homeDirectory}/.secrets/rts.sh
        fi
      ''
      else ""
    }
  '';
  profileExtra = ''
    if [ -e ${config.home.homeDirectory}/.nix-profile/etc/profile.d/nix.sh ]; then
      . ${config.home.homeDirectory}/.nix-profile/etc/profile.d/nix.sh
    fi
  '';
}
