{
  config,
  lib,
  pkgs,
  env,
  profile,
  ...
}:
let
  cfg = config.roles.system.motd;
in
{
  options = {
    roles.system.motd = {
      enable = lib.mkEnableOption "roles.system.motd";
    };
  };

  config = lib.mkIf cfg.enable (
    lib.optionalAttrs (!lib.profiles.isSystemDarwin) {
      environment.etc.motd = {
        text = with config; ''
          ${lib.common.rainbowText (
            lib.concatStrings [
              (lib.common.bannerText { font = "small"; } "hanselrd")
              (lib.common.bannerText { font = "mini"; } networking.fqdnOrHostName)
            ]
          )}

          ${lib.common.ansiText { style = "red bold"; } ''
            UNAUTHORIZED ACCESS TO THIS DEVICE IS PROHIBITED

            You must have explicit, authorized permission to access or configure this
            device. Unauthorized attempts and actions to access or use this system may
            result in civil and/or criminal penalties. All activities performed on this
            device are logged and monitored.
          ''}

          ${lib.common.ansiText { style = "gray bold"; } profile.name}: rev: ${
            lib.common.ansiText { style = "green bold"; } system.configurationRevision
          } @ ${
            lib.common.ansiText { style = "yellow bold"; } (lib.common.currentTimePretty time.timeZone)
          } by ${lib.common.ansiText { style = "cyan bold"; } env.user.username}

          host:    ${lib.common.ansiText { style = "gray bold"; } networking.fqdnOrHostName}
          os:      ${
            lib.common.ansiText {
              style = "blue bold";
            } "NixOS ${system.nixos.release} (${system.nixos.codeName})"
          }
          version: ${lib.common.ansiText { style = "magenta bold"; } system.nixos.version}
          kernel:  ${lib.common.ansiText { style = "gray bold"; } boot.kernelPackages.kernel.version}
        '';
      };

      users.motdFile = "/etc/motd";
    }
  );
}
