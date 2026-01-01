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
    ../boot-loader/grub
    ../home-manager-integration
    ../networking/networkmanager
    ../programs/fuse
    ../services/cockpit
    ../services/fail2ban
    ../services/openssh
    ../services/scx
    ../services/udisks2
    ../services/xserver
    ../virtualisation/docker
    ../zram-swap
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

  users.motd = with config; ''
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

    ${lib.x.ansiText
      {
        inherit pkgs;
        style = "red bold";
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
      lib.x.ansiText {
        inherit pkgs;
        style = "gray bold";
      } "${env.hostName}/${env.homeName}"
    }: rev: ${
      lib.x.ansiText {
        inherit pkgs;
        style = "green bold";
      } system.configurationRevision
    } @ ${
      lib.x.ansiText {
        inherit pkgs;
        style = "yellow bold";
      } (lib.x.currentTimePretty { inherit pkgs; } time.timeZone)
    } by ${
      lib.x.ansiText {
        inherit pkgs;
        style = "cyan bold";
      } env.username
    }

    host:    ${
      lib.x.ansiText {
        inherit pkgs;
        style = "gray bold";
      } networking.fqdnOrHostName
    }
    os:      ${
      lib.x.ansiText {
        inherit pkgs;
        style = "blue bold";
      } "NixOS ${system.nixos.release} (${system.nixos.codeName})"
    }
    version: ${
      lib.x.ansiText {
        inherit pkgs;
        style = "magenta bold";
      } system.nixos.version
    }
    kernel:  ${
      lib.x.ansiText {
        inherit pkgs;
        style = "gray bold";
      } boot.kernelPackages.kernel.version
    }
  '';

  system.configurationRevision = inputs.self.shortRev or "<dirty>";
}
