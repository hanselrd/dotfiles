{
  config,
  lib,
  pkgs,
  ...
}: {
  home.packages = with pkgs; [
    bun
    nodePackages.ts-node
    nodejs
  ];

  home.sessionVariables = {
    NEXT_TELEMETRY_DISABLED = 1;
  };
}
