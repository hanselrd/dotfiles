{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    /etc/nixos/hardware-configuration.nix
    ./generic.nix
  ];

  roles.system.boot.enable = true;
  roles.system.kernel.enable = true;
}
