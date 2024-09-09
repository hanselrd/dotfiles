{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    /etc/nixos/hardware-configuration.nix
  ];

  roles.system.bootstrap.enable = true;
  roles.system.boot.enable = true;
  roles.system.garuda.enable = true;
  roles.system.kernel.enable = true;
  roles.system.motd.enable = true;
  roles.system.nix.enable = true;
  roles.system.shell.enable = true;
  roles.system.sudo.enable = true;
  roles.system.user.enable = true;
  roles.system.time.enable = true;
  roles.system.x11.enable = true;
  roles.system.i18n.enable = true;
  roles.system.monitoring.enable = true;
  roles.system.networking.enable = true;
  roles.system.virtualization.enable = true;
}
