{
  config,
  lib,
  pkgs,
  env,
  ...
}: let
  cfg = config.roles.system.networking;
in {
  options = {
    roles.system.networking = {
      enable = lib.mkEnableOption "roles.system.networking";
    };
  };

  config = lib.mkIf cfg.enable {
    networking.hostName = env.roles.system.networking.hostName;

    networking.networkmanager.enable = true;

    networking.firewall.allowedTCPPorts = [5000 9443];
    networking.firewall.allowedTCPPortRanges = [
      {
        from = 3000;
        to = 3100;
      }
    ];
  };
}
