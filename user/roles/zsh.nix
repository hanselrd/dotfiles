{
  config,
  lib,
  pkgs,
  env,
  ...
}:
let
  cfg = config.roles.user.zsh;
in
{
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
      # plugins = with pkgs; [
      #   {
      #     name = "zsh-vi-mode";
      #     src = zsh-vi-mode;
      #     file = "share/zsh-vi-mode/zsh-vi-mode.plugin.zsh";
      #   }
      # ];
      initExtra = ''
        ${
          if env.roles.user.shell.theme then
            ''
              ${lib.getExe' pkgs.bash "sh"} ${
                lib.vendor.nix-colors-contrib.shellThemeFromScheme { scheme = config.colorScheme; }
              }
            ''
          else
            ""
        }
      '';
      profileExtra = ''
        if [ -e ${env.user.homeDirectory}/.nix-profile/etc/profile.d/nix.sh ]; then
          . ${env.user.homeDirectory}/.nix-profile/etc/profile.d/nix.sh
        fi
      '';
    };
  };
}
