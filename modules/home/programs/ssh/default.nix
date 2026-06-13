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
    settings = {
      "10.*.*.* 192.168.*.*" = {
        Compression = true;
        StrictHostKeyChecking = "no";
        UserKnownHostsFile = "/dev/null";
      };
      "*" = {
        # AddKeysToAgent = "no";
        Compression = false;
        ControlMaster = "no";
        ControlPath = "${config.home.homeDirectory}/.ssh/master-%r@%n:%p";
        ControlPersist = "no";
        ForwardAgent = false;
        HashKnownHosts = false;
        ServerAliveCountMax = 3;
        ServerAliveInterval = 0;
        UserKnownHostsFile = "${config.home.homeDirectory}/.ssh/known_hosts";
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
