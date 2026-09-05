{
  self,
  inputs,
  lib,
  ...
}:
let
  nixRoot = null;
in
{
  modules = self.lib.eachModule (module: {
    common =
      { config, pkgs, ... }:
      lib.mkMerge [
        {
          nix.registry = lib.mapAttrs (_: flake: { inherit flake; }) (
            lib.filterAttrs (_: lib.isType "flake") inputs
          );

          nix.settings = lib.mergeAttrsList [
            {
              experimental-features = [
                "nix-command"
                "flakes"
                "pipe-operators"
              ];
              store = lib.mkIf (
                nixRoot != null
              ) "local?store=${nixRoot}/store&state=${nixRoot}/var/nix&log=${nixRoot}/var/log/nix";
              sandbox = true;
              show-trace = true;
              plugin-files = "${pkgs.nix-plugins}/lib/nix/plugins";
              extra-builtins-file = self.outPath + "/modules/lib/_builtins.nix";
            }
            (lib.optionalAttrs (lib.elem module [
              "nixos"
              "darwin"
            ]) { auto-optimise-store = true; })
            (lib.optionalAttrs (module == "nixos") { trusted-users = [ "@wheel" ]; })
          ];

          nix.gc = lib.mergeAttrsList [
            {
              automatic = true;
              options = "--delete-older-than 7d";
            }
            (lib.optionalAttrs (lib.elem module [
              "nixos"
              "home"
            ]) { dates = "weekly"; })
            (lib.optionalAttrs (module == "darwin") {
              interval = {
                Hour = 3;
                Minute = 15;
                Weekday = 7;
              };
            })
          ];

          nix.package = lib.mkForce pkgs.nix;
        }
        (lib.optionalAttrs
          (lib.elem module [
            "nixos"
            "darwin"
          ])
          {
            nix.nixPath = [ "/etc/nix/path" ];
            environment.etc = lib.mapAttrs' (
              name: value: lib.nameValuePair "nix/path/${name}" { source = value.flake; }
            ) config.nix.registry;

            nix.optimise = lib.mergeAttrsList [
              { automatic = true; }
              (lib.optionalAttrs (module == "nixos") { dates = "weekly"; })
              (lib.optionalAttrs (module == "darwin") {
                interval = {
                  Hour = 3;
                  Minute = 15;
                  Weekday = 7;
                };
              })
            ];

            security.sudo.extraConfig = ''
              Defaults pwfeedback
              Defaults insults
            '';

            system.configurationRevision = self.shortRev or "<dirty>";
          }
        )
        (lib.optionalAttrs (module == "nixos") {
          boot.loader.efi.canTouchEfiVariables = true;

          boot.tmp.cleanOnBoot = true;

          boot.kernelPackages = pkgs.linuxPackages_latest;

          boot.kernel.sysctl = {
            "vm.overcommit_memory" = 2;
            "vm.swappiness" = 1;
            "vm.vfs_cache_pressure" = 500;
          };

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
        })
        (lib.optionalAttrs (module == "darwin") {
          networking.computerName = config.networking.hostName;
          system.defaults.smb.NetBIOSName = config.networking.hostName;

          security.pam.services.sudo_local.touchIdAuth = true;

          system.defaults.menuExtraClock.Show24Hour = true;

          homebrew = {
            enable = true;
            onActivation = {
              autoUpdate = false;
            };
            taps = [ "homebrew/services" ];
            brews = [ ];
            casks = [ ];
          };
        })
        (lib.optionalAttrs (module == "home") {
          nix.nixPath = [ "${config.xdg.configHome}/nix/path" ];
          xdg.configFile = lib.mapAttrs' (
            name: value: lib.nameValuePair "nix/path/${name}" { source = value.flake; }
          ) config.nix.registry;

          home.preferXdgDirectories = true;

          xdg.enable = true;

          # xdg.userDirs = {
          #   enable = true;
          #   createDirectories = true;
          # };

          home.shellAliases = {
            cd1 = "cd ..";
            cd2 = "cd ../..";
            cd3 = "cd ../../..";
            cd4 = "cd ../../../..";
            cd5 = "cd ../../../../..";
            rcp = "rsync -cavzP";
            rmv = "rsync -cavzP --remove-source-files";
            rrm = "mkdir -p ${config.xdg.cacheHome}/rsync/empty && rsync -avP --delete ${config.xdg.cacheHome}/rsync/empty/";
            shroot = "sudo -E $SHELL";
            sudo = "sudo ";
            vi = "vim -u NONE -U NONE -N -i NONE";
          };

          home.sessionVariables = rec {
            BROWSER = "brave";

            CS_DISABLE_FILE_DOWNLOADS = 1;
            EDITOR = "nvim";
            SUDO_EDITOR = EDITOR;
            VISUAL = EDITOR;

            PAGER = "less -s";
            MANPAGER = PAGER;

            TERMINAL = "alacritty";

            HISTTIMEFORMAT = "${self.lib.timeFormat}  ";
            # LD_LIBRARY_PATH = "$LD_LIBRARY_PATH\${LD_LIBRARY_PATH:+:}${pkgs.sssd}/lib";
          };
        })
      ];
  });
}
