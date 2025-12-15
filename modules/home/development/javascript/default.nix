{
  config,
  lib,
  pkgs,
  ...
}:
{
  home.packages = with pkgs; [
    bun
    nodejs
  ];

  home.sessionVariables = {
    NEXT_TELEMETRY_DISABLED = 1;
  };
}
