{
  config,
  lib,
  pkgs,
  ...
}:
{
  roles.system.bootstrap.enable = true;
  roles.system.docker.enable = true;
  roles.system.language.enable = true;
  roles.system.monitoring.enable = true;
  roles.system.motd.enable = true;
  roles.system.networking.enable = true;
  roles.system.nix.enable = true;
  roles.system.shell.enable = true;
  roles.system.sudo.enable = true;
  roles.system.time.enable = true;
  roles.system.user.enable = true;
  roles.system.xserver.enable = true;
}
