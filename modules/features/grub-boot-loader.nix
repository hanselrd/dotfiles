{ self, ... }: {
  modules.nixos.grub-boot-loader = {
    boot.loader.grub = {
      enable = true;
      # efiSupport = true;
      device = self.lib.builtins.getDevice "/boot";
      useOSProber = true;
    };
  };
}
