{
  self,
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.system.bootstrap;
in {
  options = {
    roles.system.bootstrap = {
      enable = lib.mkEnableOption "roles.system.bootstrap";
    };
  };

  config = lib.mkIf cfg.enable {
    system.configurationRevision = self.rev or "dirty";

    system.stateVersion = "22.05";
  };
}
