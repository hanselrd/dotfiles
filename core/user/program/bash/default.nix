{
  config,
  lib,
  pkgs,
  ...
}: {
  profileExtra = ''
    if [ -e ${config.home.homeDirectory}/.nix-profile/etc/profile.d/nix.sh ]; then . ${config.home.homeDirectory}/.nix-profile/etc/profile.d/nix.sh; fi

    # if command -v zsh &> /dev/null; then
    #   export SHELL=$(command -v zsh)
    #   exec zsh -l
    # fi
  '';
}
