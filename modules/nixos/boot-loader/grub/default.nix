{ lib, ... }:
{
  boot.loader.grub = {
    enable = true;
    # efiSupport = true;
    device = lib.x.getDevice "/boot";
    useOSProber = true;
  };
}
