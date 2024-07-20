{
  config,
  lib,
  pkgs,
  ...
}: {
  roles.system.bootstrap.enable = true;
  roles.system.wsl.enable = true;
  roles.system.motd.enable = true;
  roles.system.nix.enable = true;
  roles.system.shell.enable = true;
  roles.system.user.enable = true;
  roles.system.time.enable = true;
  roles.system.x11.enable = true;
  roles.system.i18n.enable = true;
  roles.system.monitoring.enable = true;
  roles.system.networking.enable = true;
  roles.system.virtualization.enable = true;
}
