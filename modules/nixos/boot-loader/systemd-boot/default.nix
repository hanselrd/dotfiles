{ config, ... }:
{
  boot.loader.systemd-boot = {
    enable = true;
    # xbootldrMountPoint = "/boot";
  };

  boot.loader.efi.efiSysMountPoint =
    if config.boot.loader.systemd-boot.xbootldrMountPoint != null then "/efi" else "/boot/efi";
}
