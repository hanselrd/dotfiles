{ self, lib, ... }:
let
  locales = [
    "en_US.UTF-8"
    "es_DO.UTF-8"
    "es_ES.UTF-8"
  ];
in
{
  modules = self.lib.eachModule (
    module:
    lib.listToAttrs (
      lib.map (
        locale:
        let
          charset = lib.elemAt (lib.splitString "." locale) 1;
        in
        lib.nameValuePair "${lib.replaceStrings [ "_" "." ] [ "-" "-" ] (lib.toLower locale)}-locale" (
          lib.mergeAttrsList [
            (lib.optionalAttrs (module == "nixos") {
              i18n.defaultLocale = locale;
              i18n.defaultCharset = charset;
            })
            (lib.optionalAttrs (module == "home") { home.language.base = locale; })
          ]
        )
      ) locales
    )
  );
}
