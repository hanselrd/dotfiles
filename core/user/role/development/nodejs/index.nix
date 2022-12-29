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

  home.sessionVariables = {
    NEXT_TELEMETRY_DISABLED = 1;
  };
}
