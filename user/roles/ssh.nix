{
  config,
  lib,
  pkgs,
  env,
  ...
}: let
  cfg = config.roles.user.ssh;
in {
  options = {
    roles.user.ssh = {
      enable = lib.mkEnableOption "roles.user.ssh";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.ssh = {
      enable = true;
      compression = true;
      matchBlocks = {
        "10.*.*.* 192.168.*.*" = {
          extraOptions = {
            StrictHostKeyChecking = "no";
            UserKnownHostsFile = "/dev/null";
          };
        };
      };
    };

    home.file.".ssh/.keygen" = rec {
      text = onChange;
      onChange = ''
        test -f ${env.user.homeDirectory}/.ssh/id_ed25519 || ${lib.getExe' pkgs.openssh "ssh-keygen"} -t ed25519 -a 100 -N "" -f ${env.user.homeDirectory}/.ssh/id_ed25519
        # test -f ${env.user.homeDirectory}/.ssh/id_rsa || ${lib.getExe' pkgs.openssh "ssh-keygen"} -t rsa -b 4096 -o -a 100 -N "" -f ${env.user.homeDirectory}/.ssh/id_rsa
      '';
    };
  };
}
