{
  config,
  lib,
  pkgs,
  ...
}:
{
  imports = [
    ./generic.nix
  ];

  roles.system.chocolatey.enable = true;
  roles.system.glazewm.enable = true;
  roles.system.winget.enable = true;
  roles.system.wsl.enable = true;
}
