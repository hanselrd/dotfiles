{
  config,
  lib,
  pkgs,
  ...
}: {
  programs.alacritty = lib.ext.mkProgram "alacritty" {};

  home.sessionVariables = {
    TERMINAL = "alacritty";
  };
}
