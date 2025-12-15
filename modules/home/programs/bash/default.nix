{
  config,
  lib,
  pkgs,
  env,
  ...
}:
{
  programs.bash = {
    enable = true;
    profileExtra = ''
      if [ -e ${config.home.homeDirectory}/.nix-profile/etc/profile.d/nix.sh ]; then
        . ${config.home.homeDirectory}/.nix-profile/etc/profile.d/nix.sh
      fi
    '';
  };
}
