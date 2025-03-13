{
  config,
  lib,
  pkgs,
  ...
}:
{
  imports = [
    ./standard.nix
  ];

  roles.user.bottles.enable = true;
  roles.user.browser.enable = true;
  roles.user.flameshot.enable = true;
  roles.user.fonts.enable = true;
  roles.user.gaming.enable = true;
  roles.user.redshift.enable = true;
  roles.user.rofi.enable = true;
  roles.user.terminal.enable = true;
  roles.user.vscode.enable = true;
  roles.user.wine.enable = true;
  roles.user.wiztree.enable = true;
}
