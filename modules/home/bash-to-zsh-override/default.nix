{ lib, ... }:
{
  programs.bash.profileExtra = lib.mkAfter ''
    if command -v zsh &> /dev/null; then
      export SHELL=$(command -v zsh)
      exec zsh -l
    fi
  '';
}
