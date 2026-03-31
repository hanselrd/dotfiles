{ pkgs, ... }:
{
  home.packages = with pkgs; [
    bun
    nodejs
    typescript
    typescript-language-server
  ];

  home.sessionVariables = {
    NEXT_TELEMETRY_DISABLED = 1;
  };
}
