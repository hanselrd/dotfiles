{
  config,
  lib,
  pkgs,
  ...
}: {
  home.packages = with pkgs; [
    less
  ];

  home.sessionVariables = rec {
    PAGER = "less -s";
    MANPAGER = PAGER;
  };
}
