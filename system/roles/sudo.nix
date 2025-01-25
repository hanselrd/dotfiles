{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.roles.system.sudo;
in
{
  options = {
    roles.system.sudo = {
      enable = lib.mkEnableOption "roles.system.sudo";
    };
  };

  config = lib.mkIf cfg.enable {
    security.sudo.extraConfig = ''
      Defaults pwfeedback
      Defaults insults
    '';
  };
}
