{
  config,
  lib,
  pkgs,
  ...
}: {
  programs.go = lib.core.user.mkProgram "go" {};
}
