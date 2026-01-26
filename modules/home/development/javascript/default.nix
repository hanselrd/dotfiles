{ pkgs, ... }:
{
  home.packages = with pkgs; [
    bun
    nodePackages.typescript
    nodePackages.typescript-language-server
    nodejs
  ];

  home.sessionVariables = {
    NEXT_TELEMETRY_DISABLED = 1;
  };
}
