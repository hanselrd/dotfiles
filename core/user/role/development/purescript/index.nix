{
  config,
  lib,
  pkgs,
  ...
}: {
  home.packages = with pkgs; [
    purescript
    # spago
  ];
}
