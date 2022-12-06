{
  config,
  lib,
  pkgs,
  ...
}: {
  home.packages = with pkgs; [
    nodejs
    nodePackages.ts-node
  ];
}
