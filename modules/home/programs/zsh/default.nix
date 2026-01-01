{ config, ... }:
{
  programs.zsh = {
    enable = true;
    dotDir = config.home.homeDirectory;
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
    profileExtra = ''
      if [ -e ${config.home.homeDirectory}/.nix-profile/etc/profile.d/nix.sh ]; then
        . ${config.home.homeDirectory}/.nix-profile/etc/profile.d/nix.sh
      fi
    '';
  };
}
