{
  config,
  lib,
  pkgs,
  ...
}:
{
  programs.fuse = {
    enable = true;
    userAllowOther = true;
  };
}
