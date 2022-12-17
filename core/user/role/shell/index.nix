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

  programs.exa = lib.core.user.mkProgram "exa" {enableAliases = false;};

  home.shellAliases = {
    la = "exa -a";
    ll = "exa -l --octal-permissions";
    lla = "exa -la --octal-permissions";
    ls = "exa";
    lt = "exa --tree";
  };
}
