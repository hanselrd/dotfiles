{
  config,
  lib,
  pkgs,
  ...
}: {
  programs.alacritty = lib.core.user.mkProgram "alacritty" {};

  home.sessionVariables = {
    TERMINAL = "alacritty";
  };
}
