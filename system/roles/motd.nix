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
      users.motd = with config; ''

        888                                          888              888
        888-~88e   /~~~8e  888-~88e  d88~\  e88~~8e  888 888-~\  e88~\888
        888  888       88b 888  888 C888   d888  88b 888 888    d888  888
        888  888  e88~-888 888  888  Y88b  8888__888 888 888    8888  888
        888  888 C888  888 888  888   888D Y888    , 888 888    Y888  888
        888  888  "88_-888 888  888 \_88P   "88___/  888 888     "88_/888

                 UNAUTHORIZED ACCESS TO THIS DEVICE IS PROHIBITED

        You must have explicit, authorized permission to access or configure this
        device. Unauthorized attempts and actions to access or use this system may
        result in civil and/or criminal penalties. All activities performed on this
        device are logged and monitored.

        ${profile.name}: rev: ${system.configurationRevision} @ ${lib.common.currentTimePretty time.timeZone} by ${env.user.username}

        Host:    ${networking.fqdnOrHostName}
        OS:      NixOS ${system.nixos.release} (${system.nixos.codeName})
        Version: ${system.nixos.version}
        Kernel:  ${boot.kernelPackages.kernel.version}

      '';
    }
  );
}
