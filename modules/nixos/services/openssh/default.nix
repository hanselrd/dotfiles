{
  config,
  lib,
  pkgs,
  ...
}:
{
  services.openssh = {
    enable = true;
    settings = {
      X11Forwarding = config.services.xserver.enable;
      PrintLastLog = false;
    };
  };
}
