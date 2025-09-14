{
  config,
  lib,
  pkgs,
  env,
  ...
}:
let
  cfg = config.roles.system.language;
in
{
  options = {
    roles.system.language = {
      enable = lib.mkEnableOption "roles.system.language";
    };
  };

  config = lib.mkIf cfg.enable (
    lib.optionalAttrs (!lib.profiles.isSystemDarwin) {
      i18n.defaultLocale = env.roles.system.language.locale;
      i18n.defaultCharset = env.roles.system.language.charset;
      i18n.extraLocales = env.roles.system.language.extraLocales;

      home-manager.users.${env.user.username}.home.language.base = env.roles.system.language.locale;
    }
  );
}
