{
  config,
  lib,
  pkgs,
  ...
}: {
  programs.bat = lib.core.user.mkProgram "bat" {};

  programs.exa = lib.core.user.mkProgram "exa" {enableAliases = false;};

  programs.fzf = lib.core.user.mkProgram "fzf" {};

  programs.git = lib.core.user.mkProgram "git" {};

  # programs.gpg = lib.core.user.mkProgram "gpg" {};

  programs.ssh = lib.core.user.mkProgram "ssh" {};

  programs.tmux = lib.core.user.mkProgram "tmux" {};

  # services.gpg-agent = lib.core.user.mkService "gpg-agent" {};

  home.shellAliases = {
    la = "exa -a";
    ll = "exa -l --octal-permissions";
    lla = "exa -la --octal-permissions";
    ls = "exa";
    lt = "exa --tree";
    lta = "exa --tree -a";
  };
}
