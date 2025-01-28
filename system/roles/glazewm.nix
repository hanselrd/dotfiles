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
        "glazewm0" = {
          text = ''
            ${lib.getExe pkgs.flock} winget -c "winget.exe install --exact --id --disable-interactivity lars-berger.GlazeWM"
            ${lib.getExe' pkgs.coreutils "install"} -DT ${./glazewm/config.yaml} ${
              lib.escape [ " " ] env.extra.winUser.userProfile
            }/.glzr/glazewm/config.yaml
          '';
        };
      };
    }
  );
}
