{
  config,
  lib,
  pkgs,
  env,
  ...
}:
{
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
    profileExtra = config.programs.bash.profileExtra;
  };
}
