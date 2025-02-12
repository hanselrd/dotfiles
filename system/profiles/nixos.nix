{
  config,
  lib,
  pkgs,
  ...
}:
{
  imports = [
    /etc/nixos/hardware-configuration.nix
    ./generic.nix
  ];

  roles.system.boot.enable = true;
  roles.system.hyprland.enable = true;
  roles.system.kernel.enable = true;
  roles.system.udisks2.enable = true;
  roles.system.zram.enable = true;
}
