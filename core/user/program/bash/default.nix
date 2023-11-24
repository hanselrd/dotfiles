{
  config,
  lib,
  pkgs,
  env,
  ...
}: {
  initExtra = ''
    ${
      if env.roles.user.shell.theme
      then ''
        sh ${lib.vendor.nix-colors-contrib.shellThemeFromScheme {scheme = config.colorScheme;}}
      ''
      else ""
    }

    ${
      if env.roles.user.shell.rts
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

    ${
      if env.roles.user.shell.bashToZsh
      then ''
        if command -v zsh &> /dev/null; then
          export SHELL=$(command -v zsh)
          exec zsh -l
        fi
      ''
      else ""
    }
  '';
}
