{ config, lib, ... }:
{
  services.xrdp = {
    enable = true;
    defaultWindowManager = lib.mkIf config.services.desktopManager.plasma6.enable "startplasma-x11";
    openFirewall = true;
  };
}
