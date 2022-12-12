{
  config,
  lib,
  pkgs,
  ...
}: {
  compression = true;
  matchBlocks = {
    "10.*.*.* 192.168.*.*" = {
      extraOptions = {
        StrictHostKeyChecking = "no";
        UserKnownHostsFile = "/dev/null";
      };
    };
  };
}
