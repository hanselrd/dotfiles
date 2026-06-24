{ pkgs, ... }: {
  home.packages = with pkgs; [
    amber-lang
    bc
  ];
}
