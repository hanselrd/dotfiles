{
  config,
  lib,
  pkgs,
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
  };
}
