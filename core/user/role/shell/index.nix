{
  config,
  lib,
  pkgs,
  ...
}: {
  programs.bash = lib.ext.mkProgram "bash" {};

  programs.zsh = lib.ext.mkProgram "zsh" {};

  programs.starship = lib.ext.mkProgram "starship" {};

  programs.fzf = lib.ext.mkProgram "fzf" {};
}
