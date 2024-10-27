{
  config,
  lib,
  pkgs,
  env,
  ...
}: let
  cfg = config.roles.user.brave;
in {
  options = {
    roles.user.brave = {
      enable = lib.mkEnableOption "roles.user.brave";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      brave
    ];

    home.file = lib.mkIf (!env.extra.encrypted.private) {
      ".tmp/brave0" = lib.common.runExternalAlways ''
        ${lib.getExe' pkgs.coreutils "install"} -DT -m 600 ${../../secrets/private/user/roles/brave/Bookmarks} ${env.user.configDirectory}/BraveSoftware/Brave-Browser/Default/Bookmarks
      '';
      ".tmp/brave1" = lib.mkIf lib.profiles.isSystemDarwin (
        lib.common.runExternalAlways ''
          ${lib.getExe' pkgs.coreutils "install"} -DT -m 600 ${../../secrets/private/user/roles/brave/Bookmarks} ${lib.concatMapStringsSep "/" (x: lib.escape [" "] x) [env.user.homeDirectory "Library/Application Support/BraveSoftware/Brave-Browser/Default/Bookmarks"]}
        ''
      );
      ".tmp/brave2" = lib.mkIf lib.profiles.isSystemWsl (
        lib.common.runExternalAlways ''
          ${lib.getExe' pkgs.coreutils "install"} -DT ${../../secrets/private/user/roles/brave/Bookmarks} ${lib.concatMapStringsSep "/" (x: lib.escape [" "] x) [env.extra.winUser.localAppData "BraveSoftware/Brave-Browser/User Data/Default/Bookmarks"]}
        ''
      );
    };
  };
}
