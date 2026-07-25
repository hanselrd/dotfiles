{
  inputs,
  config,
  lib,
  pkgs,
  sharedModulesPath,
  env,
  ...
}:
{
  imports = with inputs; [
    agenix.nixosModules.default
    disko.nixosModules.disko
    impermanence.nixosModules.impermanence
    stylix.nixosModules.stylix
    (sharedModulesPath + "/common")
    # ../services/cockpit
    # ../services/xserver
    ../boot-loader/grub
    ../home-manager-integration
    ../networking/networkmanager
    ../programs/fuse
    ../services/fail2ban
    ../services/openssh
    ../services/scx
    ../services/udisks2
    ../swap
    ../virtualisation/docker
  ];

  networking.hostName = env.hostName;

  nix.nixPath = [ "/etc/nix/path" ];
  environment.etc = lib.mapAttrs' (name: value: {
    name = "nix/path/${name}";
    value.source = value.flake;
  }) config.nix.registry;

  nix.settings = {
    auto-optimise-store = true;
    trusted-users = [ "@wheel" ];
  };

  nix.optimise = {
    automatic = true;
    dates = "weekly";
  };

  boot.loader.efi.canTouchEfiVariables = true;

  boot.tmp.cleanOnBoot = true;

  boot.kernelPackages = pkgs.linuxPackages_latest;

  boot.kernel.sysctl = {
    "vm.swappiness" = 1;
    "vm.vfs_cache_pressure" = 500;
  };

  users.users.${env.username} = {
    description = env.name;
    isNormalUser = true;
    uid = 1000;
    extraGroups = lib.flatten [
      "wheel"
      (lib.optional config.virtualisation.docker.enable "docker")
      (lib.optional config.networking.networkmanager.enable "networkmanager")
    ];
    initialPassword = "password";
    shell = pkgs.zsh;
  };

  programs.zsh.enable = true;

  environment.shells = with pkgs; [ zsh ];

  users.defaultUserShell = pkgs.zsh;

  i18n.defaultLocale = "en_US.UTF-8";
  i18n.defaultCharset = "UTF-8";
  i18n.extraLocales = [
    "es_DO.UTF-8/UTF-8"
    "es_ES.UTF-8/UTF-8"
  ];

  networking.firewall = {
    allowedTCPPorts = [
      5000
      9443
    ];
    allowedTCPPortRanges = [
      {
        from = 3000;
        to = 3100;
      }
    ];
  };

  security.sudo.extraConfig = ''
    Defaults pwfeedback
    Defaults insults
  '';

  time.timeZone = env.timeZone;

  users.motd =
    let
      inherit (config)
        boot
        networking
        system
        time
        ;
    in
    with config.lib.stylix.colors.withHashtag;
    ''
      ${lib.x.rainbowText { inherit pkgs; } (
        lib.concatStrings [
          (lib.x.bannerText {
            inherit pkgs;
            font = "small";
          } "hanselrd")
          (lib.x.bannerText {
            inherit pkgs;
            font = "mini";
          } networking.fqdnOrHostName)
        ]
      )}

      ${lib.x.pastelText
        {
          inherit pkgs;
          fgColor = bright-red;
          bold = true;
        }
        ''
          UNAUTHORIZED ACCESS TO THIS DEVICE IS PROHIBITED

          You must have explicit, authorized permission to access or configure this
          device. Unauthorized attempts and actions to access or use this system may
          result in civil and/or criminal penalties. All activities performed on this
          device are logged and monitored.
        ''
      }

      ${
        lib.x.pastelText {
          inherit pkgs;
          fgColor = "gray";
          bold = true;
        } "${env.hostName}/${env.homeName}"
      }: rev: ${
        lib.x.pastelText {
          inherit pkgs;
          fgColor = bright-green;
          bold = true;
        } system.configurationRevision
      } @ ${
        lib.x.pastelText {
          inherit pkgs;
          fgColor = bright-magenta;
          bold = true;
        } (lib.x.currentTimePretty { inherit pkgs; } time.timeZone)
      } by ${
        lib.x.pastelText {
          inherit pkgs;
          fgColor = bright-cyan;
          bold = true;
        } env.username
      }

      host:    ${
        lib.x.pastelText {
          inherit pkgs;
          fgColor = "gray";
          bold = true;
        } networking.fqdnOrHostName
      }
      os:      ${
        lib.x.pastelText {
          inherit pkgs;
          fgColor = bright-blue;
          bold = true;
        } "NixOS ${system.nixos.release} (${system.nixos.codeName})"
      }
      version: ${
        lib.x.pastelText {
          inherit pkgs;
          fgColor = bright-magenta;
          bold = true;
        } system.nixos.version
      }
      kernel:  ${
        lib.x.pastelText {
          inherit pkgs;
          fgColor = "gray";
          bold = true;
        } boot.kernelPackages.kernel.version
      }
      nix:     ${
        lib.x.pastelText {
          inherit pkgs;
          fgColor = bright-blue;
          bold = true;
        } pkgs.nix.version
      }
    '';

  system.configurationRevision = inputs.self.shortRev or "<dirty>";
}
