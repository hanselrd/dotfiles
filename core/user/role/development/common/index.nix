{
  config,
  lib,
  pkgs,
  ...
}: {
  home.packages = with pkgs; [
    ctags
    gnumake
    gnupatch
  ];
}
