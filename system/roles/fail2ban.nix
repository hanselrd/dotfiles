{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.roles.system.fail2ban;
in
{
  options = {
    roles.system.fail2ban = {
      enable = lib.mkEnableOption "roles.system.fail2ban";
    };
  };

  config = lib.mkIf cfg.enable (
    lib.optionalAttrs (!lib.profiles.isSystemDarwin) {
      services.fail2ban.enable = true;
    }
  );
}
