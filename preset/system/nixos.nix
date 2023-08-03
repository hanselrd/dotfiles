# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  self,
  config,
  lib,
  pkgs,
  preset,
  ...
}: {
  imports = [
    # Include the results of the hardware scan.
    /etc/nixos/hardware-configuration.nix
  ];

  # Set configuration revision
  system.configurationRevision = self.rev or "dirty";

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";

  # Configure tmp
  boot.tmp.cleanOnBoot = true;

  networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  # Enable virtualisation
  virtualisation.docker.enable = true;

  # Set your time zone.
  time.timeZone = "America/New_York";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.utf8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  # # Enable the X11 windowing system.
  # services.xserver.enable = true;

  # # Enable the KDE Plasma Desktop Environment.
  # services.xserver.displayManager.sddm.enable = true;
  # services.xserver.desktopManager.plasma5.enable = true;

  # # Enable RDP
  # services.xrdp.enable = true;
  # services.xrdp.defaultWindowManager = "startplasma-x11";
  # services.xrdp.openFirewall = true;

  # Configure keymap in X11
  services.xserver = {
    layout = "us";
    xkbVariant = "";
  };

  # Enable shell system-wide
  programs.zsh.enable = true;
  environment.shells = with pkgs; [zsh];

  # Change shell system-wide
  users.defaultUserShell = pkgs.zsh;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.${pkgs.config.home.username} = {
    isNormalUser = true;
    description = pkgs.config.home.name;
    extraGroups = ["networkmanager" "wheel" "docker"];
    initialPassword = "password";
    # shell = pkgs.zsh;
  };

  # Change MOTD
  users.motd = with config; ''

    888                                          888              888
    888-~88e   /~~~8e  888-~88e  d88~\  e88~~8e  888 888-~\  e88~\888
    888  888       88b 888  888 C888   d888  88b 888 888    d888  888
    888  888  e88~-888 888  888  Y88b  8888__888 888 888    8888  888
    888  888 C888  888 888  888   888D Y888    , 888 888    Y888  888
    888  888  "88_-888 888  888 \_88P   "88___/  888 888     "88_/888

             UNAUTHORIZED ACCESS TO THIS DEVICE IS PROHIBITED

    You must have explicit, authorized permission to access or configure this
    device. Unauthorized attempts and actions to access or use this system may
    result in civil and/or criminal penalties. All activities performed on this
    device are logged and monitored.

    ${preset.system}-${preset.user}: rev: ${self.shortRev or "dirty"} @ ${lib.core.currentTimeUtcPretty} by ${pkgs.config.home.username}

    Host:    ${networking.hostName}
    OS:      NixOS ${system.nixos.release} (${system.nixos.codeName})
    Version: ${system.nixos.version}
    Kernel:  ${boot.kernelPackages.kernel.version}

  '';

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    git
    vim
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    settings = {
      X11Forwarding = true;
    };
  };

  # Enable Cockpit
  services.cockpit = {
    enable = true;
    openFirewall = true;
  };

  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [5000 9443];
  networking.firewall.allowedTCPPortRanges = [
    {
      from = 3000;
      to = 3100;
    }
  ];
  networking.firewall.allowedUDPPorts = [51820];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.05"; # Did you read the comment?
}
