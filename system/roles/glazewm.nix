{
  config,
  lib,
  pkgs,
  env,
  ...
}:
let
  cfg = config.roles.system.glazewm;
in
{
  options = {
    roles.system.glazewm = {
      enable = lib.mkEnableOption "roles.system.glazewm";
    };
  };

  config = lib.mkIf cfg.enable (
    lib.optionalAttrs lib.profiles.isSystemWsl {
      system.activationScripts = {
        glazewm0 = lib.common.runExternalSystem "glazewm0" (
          lib.common.winGetInstallExternal "lars-berger.GlazeWM" { }
        );
        glazewm1 = lib.common.runExternalSystem "glazewm1" {
          text = ''
            ${lib.getExe' pkgs.coreutils "install"} -DT ${./glazewm/config.yaml} ${
              lib.escape [ " " ] env.extra.winUser.userProfile
            }/.glzr/glazewm/config.yaml
          '';
          runAlways = true;
          deps = [ "glazewm0" ];
        };
      };
    }
  );
}
