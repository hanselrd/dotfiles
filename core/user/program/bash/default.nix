{
  config,
  lib,
  pkgs,
  ...
}: {
  initExtra = ''
    # sh ${lib.vendor.nix-colors-contrib.shellThemeFromScheme {scheme = config.colorScheme;}}

    if [ -e ${config.home.homeDirectory}/.secrets/rts.sh ]; then
      . ${config.home.homeDirectory}/.secrets/rts.sh
    fi
  '';
  profileExtra = ''
    if [ -e ${config.home.homeDirectory}/.nix-profile/etc/profile.d/nix.sh ]; then
      . ${config.home.homeDirectory}/.nix-profile/etc/profile.d/nix.sh
    fi

    # if command -v zsh &> /dev/null; then
    #   export SHELL=$(command -v zsh)
    #   exec zsh -l
    # fi
  '';
}
