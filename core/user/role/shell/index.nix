{
  config,
  lib,
  pkgs,
  ...
}: {
  programs.bash = lib.core.user.mkProgram "bash" {};

  programs.zsh = lib.core.user.mkProgram "zsh" {};

  programs.starship = lib.core.user.mkProgram "starship" {};

  programs.fzf = lib.core.user.mkProgram "fzf" {};
}
