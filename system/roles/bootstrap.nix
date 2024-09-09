{
  inputs,
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.system.bootstrap;
  inherit (inputs) self;
in {
  options = {
    roles.system.bootstrap = {
      enable = lib.mkEnableOption "roles.system.bootstrap";
    };
  };

  config = lib.mkIf cfg.enable {
    roles.system.home-manager.enable = true;

    system.configurationRevision = self.shortRev or "<dirty>";

    system.stateVersion =
      if !lib.profiles.isSystemDarwin
      then "22.05"
      else 4;
  };
}
