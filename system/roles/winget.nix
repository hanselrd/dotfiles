{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.roles.system.winget;
in
{
  options = {
    roles.system.winget = {
      enable = lib.mkEnableOption "roles.system.winget";
    };
  };

  config = lib.mkIf cfg.enable (
    lib.optionalAttrs lib.profiles.isSystemWsl {
      system.activationScripts = {
        "winget0" = {
          text = ''
            ${lib.getExe pkgs.flock} winget -c "winget.exe update --all --uninstall-previous --disable-interactivity"
          '';
        };
      };
    }
  );
}
