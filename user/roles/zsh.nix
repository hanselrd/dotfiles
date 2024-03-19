{
  config,
  lib,
  pkgs,
  env,
  ...
}: let
  cfg = config.roles.user.zsh;
in {
  options = {
    roles.user.zsh = {
      enable = lib.mkEnableOption "roles.user.zsh";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.zsh = {
      enable = true;
      defaultKeymap = "emacs";
      autosuggestion = {
        enable = true;
      };
      enableCompletion = false;
      syntaxHighlighting = {
        enable = true;
        # styles = {
        #   "main" = "";
        #   "brackets" = "";
        #   "pattern" = "";
        #   "regexp" = "";
        #   "cursor" = "";
        #   "root" = "";
        #   "line" = "";
        # };
      };
      history = {
        expireDuplicatesFirst = true;
      };
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
      '';
    };
  };
}
