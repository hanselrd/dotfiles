{
  config,
  lib,
  pkgs,
  env,
  ...
}:
let
  cfg = config.roles.user.ssh;
in
{
  options = {
    roles.user.ssh = {
      enable = lib.mkEnableOption "roles.user.ssh";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.ssh = {
      enable = true;
      enableDefaultConfig = false;
      matchBlocks = {
        "*" = {
          # addKeysToAgent = "no";
          compression = false;
          controlMaster = "no";
          controlPath = "${env.user.homeDirectory}/.ssh/master-%r@%n:%p";
          controlPersist = "no";
          forwardAgent = false;
          hashKnownHosts = false;
          serverAliveCountMax = 3;
          serverAliveInterval = 0;
          userKnownHostsFile = "${env.user.homeDirectory}/.ssh/known_hosts";
        };
        "10.*.*.* 192.168.*.*" = {
          compression = true;
          userKnownHostsFile = "/dev/null";
          extraOptions = {
            StrictHostKeyChecking = "no";
          };
        };
      };
    };

    home.activation = {
      ssh0 = lib.common.runExternalHome "ssh0" {
        text = ''
          test -f ${env.user.homeDirectory}/.ssh/id_ed25519 || ${lib.getExe' pkgs.openssh "ssh-keygen"} -t ed25519 -a 100 -N "" -f ${env.user.homeDirectory}/.ssh/id_ed25519
          # test -f ${env.user.homeDirectory}/.ssh/id_rsa || ${lib.getExe' pkgs.openssh "ssh-keygen"} -t rsa -b 4096 -o -a 100 -N "" -f ${env.user.homeDirectory}/.ssh/id_rsa
        '';
      };
      ssh1 = lib.mkIf lib.profiles.isSystemWsl (
        lib.common.runExternalHome "ssh1" {
          text = ''
            ${lib.getExe' pkgs.coreutils "install"} -D ${env.user.homeDirectory}/.ssh/config ${
              lib.escape [ " " ] env.extra.winUser.homeDirectory
            }/.ssh
          '';
          runAlways = true;
        }
      );
    };
  };
}
