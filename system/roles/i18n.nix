{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.system.i18n;
in {
  options = {
    roles.system.i18n = {
      enable = lib.mkEnableOption "roles.system.i18n";
    };
  };

  config = lib.mkIf cfg.enable (
    lib.optionalAttrs (!lib.profiles.isSystemDarwin) {
      i18n.defaultLocale = "en_US.utf8";

      i18n.extraLocaleSettings = {
        LC_ADDRESS = "en_US.UTF-8";
        LC_IDENTIFICATION = "en_US.UTF-8";
        LC_MEASUREMENT = "en_US.UTF-8";
        LC_MONETARY = "en_US.UTF-8";
        LC_NAME = "en_US.UTF-8";
        LC_NUMERIC = "en_US.UTF-8";
        LC_PAPER = "en_US.UTF-8";
        LC_TELEPHONE = "en_US.UTF-8";
        LC_TIME = "en_US.UTF-8";
      };
    }
  );
}
