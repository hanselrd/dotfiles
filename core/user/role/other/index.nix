{
  config,
  lib,
  pkgs,
  ...
}: {
  programs.bat = lib.core.user.mkProgram "bat" {};

  programs.exa = lib.core.user.mkProgram "exa" {};

  programs.fzf = lib.core.user.mkProgram "fzf" {};

  programs.git = lib.core.user.mkProgram "git" {};

  # programs.gpg = lib.core.user.mkProgram "gpg" {};

  programs.ssh = lib.core.user.mkProgram "ssh" {};

  programs.tmux = lib.core.user.mkProgram "tmux" {};

  # services.gpg-agent = lib.core.user.mkService "gpg-agent" {};

  # home.packages = with pkgs; [];
}
