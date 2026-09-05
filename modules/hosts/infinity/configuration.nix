{ self, ... }: {
  modules.nixos.infinity-host = {
    imports = [
      ./_hardware-configuration.nix

      # self.modules.nixos.agenix-integration
      # self.modules.nixos.disko-integration
      # self.modules.nixos.impermanence-integration
      self.modules.nixos.home-manager-integration
      self.modules.nixos.stylix-integration

      self.modules.nixos.america-new-york-time-zone
      self.modules.nixos.common
      self.modules.nixos.delacruz-data-samba-share
      self.modules.nixos.delacruz-infinity-home
      self.modules.nixos.delacruz-motd
      self.modules.nixos.delacruz-user
      self.modules.nixos.docker-virtualisation
      self.modules.nixos.en-us-utf-8-locale
      self.modules.nixos.grub-boot-loader
      self.modules.nixos.networkmanager-networking
      self.modules.nixos.swap

      # self.modules.nixos.hyprland-desktop
      # self.modules.nixos.kde-desktop
      self.modules.nixos.fuse-program
      self.modules.nixos.zsh-program

      # self.modules.nixos.cockpit-service
      # self.modules.nixos.xrdp-service
      # self.modules.nixos.xserver-service
      self.modules.nixos.fail2ban-service
      self.modules.nixos.openssh-service
      self.modules.nixos.qemu-guest-service
      self.modules.nixos.scx-service
      self.modules.nixos.udisks2-service
    ];

    networking.hostName = "infinity";

    system.stateVersion = "26.05";
  };

  nixosConfigurations.infinity = self.lib.mkNixosConfiguration self.modules.nixos.infinity-host;
}
