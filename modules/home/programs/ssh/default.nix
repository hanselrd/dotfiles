{
  config,
  lib,
  pkgs,
  ...
}:
{
  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;
    matchBlocks = {
      "*" = {
        # addKeysToAgent = "no";
        compression = false;
        controlMaster = "no";
        controlPath = "${config.home.homeDirectory}/.ssh/master-%r@%n:%p";
        controlPersist = "no";
        forwardAgent = false;
        hashKnownHosts = false;
        serverAliveCountMax = 3;
        serverAliveInterval = 0;
        userKnownHostsFile = "${config.home.homeDirectory}/.ssh/known_hosts";
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
    ssh0 = lib.x.runExternalHome "ssh0" {
      inherit config pkgs;
      text = ''
        test -f ${config.home.homeDirectory}/.ssh/id_ed25519 || ${lib.getExe' pkgs.openssh "ssh-keygen"} -t ed25519 -a 100 -N "" -f ${config.home.homeDirectory}/.ssh/id_ed25519
        # test -f ${config.home.homeDirectory}/.ssh/id_rsa || ${lib.getExe' pkgs.openssh "ssh-keygen"} -t rsa -b 4096 -o -a 100 -N "" -f ${config.home.homeDirectory}/.ssh/id_rsa
      '';
    };
  };
}
