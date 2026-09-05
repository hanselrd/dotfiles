{
  modules.nixos.systemd-boot-boot-loader = { config, ... }: {
    boot.loader.systemd-boot = {
      enable = true;
      # xbootldrMountPoint = "/boot";
    };

    boot.loader.efi = {
      canTouchEfiVariables = true;
      efiSysMountPoint =
        if config.boot.loader.systemd-boot.xbootldrMountPoint != null then "/efi" else "/boot/efi";
    };
  };
}
