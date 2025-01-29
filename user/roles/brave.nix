{
  config,
  lib,
  pkgs,
  env,
  ...
}:
let
  cfg = config.roles.user.brave;
in
{
  options = {
    roles.user.brave = {
      enable = lib.mkEnableOption "roles.user.brave";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      brave
    ];

    home.activation = lib.mkIf (!env.extra.encrypted.blue) {
      brave0 = lib.common.runExternalHome "brave0" {
        text = ''
          ${lib.getExe' pkgs.coreutils "install"} -DT -m 600 ${../../secrets/blue/user/roles/brave/Bookmarks} ${env.user.configDirectory}/BraveSoftware/Brave-Browser/Default/Bookmarks
          ${
            if lib.profiles.isSystemDarwin then
              ''
                ${lib.getExe' pkgs.coreutils "install"} -DT -m 600 ${../../secrets/blue/user/roles/brave/Bookmarks} ${
                  lib.concatMapStringsSep "/" (x: lib.escape [ " " ] x) [
                    env.user.homeDirectory
                    "Library/Application Support/BraveSoftware/Brave-Browser/Default/Bookmarks"
                  ]
                }
              ''
            else if lib.profiles.isSystemWsl then
              ''
                ${lib.getExe' pkgs.coreutils "install"} -DT ${../../secrets/blue/user/roles/brave/Bookmarks} ${
                  lib.concatMapStringsSep "/" (x: lib.escape [ " " ] x) [
                    env.extra.winUser.localAppData
                    "BraveSoftware/Brave-Browser/User Data/Default/Bookmarks"
                  ]
                }
              ''
            else
              ""
          }
        '';
        runAlways = true;
      };
    };
  };
}
