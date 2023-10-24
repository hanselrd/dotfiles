{
  config,
  lib,
  pkgs,
  ...
}: {
  programs.go = lib.core.user.mkProgram "go" {};

  home.packages = with pkgs; [
    cobra-cli
    golines
    gotools
  ];
}
