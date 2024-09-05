{
  config,
  lib,
  pkgs,
  env,
  ...
}: let
  cfg = config.roles.user.bash;
in {
  options = {
    roles.user.bash = {
      enable = lib.mkEnableOption "roles.user.bash";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.bash = {
      enable = true;
      initExtra = ''
        ${
          if env.roles.user.shell.theme
          then ''
            sh ${lib.vendor.nix-colors-contrib.shellThemeFromScheme {scheme = config.colorScheme;}}
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
    };
  };
}
