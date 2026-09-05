{ self, lib, ... }:
let
  timeZones = [
    "America/New_York"
    "Etc/UTC"
  ];
in
{
  modules = self.lib.eachModule (
    module:
    lib.listToAttrs (
      lib.map (
        timeZone:
        lib.nameValuePair "${lib.replaceStrings [ "/" "_" ] [ "-" "-" ] (lib.toLower timeZone)}-time-zone" (
          lib.mergeAttrsList [
            (lib.optionalAttrs (lib.elem module [
              "nixos"
              "darwin"
            ]) { time.timeZone = timeZone; })
            (lib.optionalAttrs (module == "home") {
              home.sessionVariables = {
                TZ = timeZone;
                TZDIR = "\${TZDIR:-/usr/share/zoneinfo}";
              };
            })
          ]
        )
      ) timeZones
    )
  );
}
