{
  config,
  lib,
  pkgs,
  env,
  ...
}:
let
  cfg = config.roles.system.i18n;
in
{
  options = {
    roles.system.i18n = {
      enable = lib.mkEnableOption "roles.system.i18n";
    };
  };

  config = lib.mkIf cfg.enable (
    lib.optionalAttrs (!lib.profiles.isSystemDarwin) {
      i18n.defaultLocale = env.roles.system.i18n.locale;
      i18n.defaultCharset = env.roles.system.i18n.charset;
      i18n.extraLocales = env.roles.system.i18n.extraLocales;

      home-manager.users.${env.user.username}.home.language.base = env.roles.system.i18n.locale;
    }
  );
}
