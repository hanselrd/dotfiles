{ homeModulesPath, ... }:
{
  imports = [ (homeModulesPath + "/common") ];

  # home.activation = lib.mkIf (!env.extra.encrypted.blue) {
  #   brave0 = lib.mkIf lib.profiles.isSystemWsl (
  #     lib.common.runExternalHome "brave0" (lib.common.winGetInstallExternal "Brave.Brave" { })
  #   );
  #   brave1 = lib.common.runExternalHome "brave1" {
  #     text = ''
  #       ${lib.getExe' pkgs.coreutils "install"} -DT -m 600 ${../../secrets/blue/user/roles/brave/Bookmarks} ${env.user.configDirectory}/BraveSoftware/Brave-Browser/Default/Bookmarks
  #       ${
  #         if lib.profiles.isSystemDarwin then
  #           ''
  #             ${lib.getExe' pkgs.coreutils "install"} -DT -m 600 ${../../secrets/blue/user/roles/brave/Bookmarks} ${
  #               lib.concatMapStringsSep "/" (x: lib.escape [ " " ] x) [
  #                 env.user.homeDirectory
  #                 "Library/Application Support/BraveSoftware/Brave-Browser/Default/Bookmarks"
  #               ]
  #             }
  #           ''
  #         else if lib.profiles.isSystemWsl then
  #           ''
  #             ${lib.getExe' pkgs.coreutils "install"} -DT ${../../secrets/blue/user/roles/brave/Bookmarks} ${
  #               lib.concatMapStringsSep "/" (x: lib.escape [ " " ] x) [
  #                 env.extra.winUser.localAppData
  #                 "BraveSoftware/Brave-Browser/User Data/Default/Bookmarks"
  #               ]
  #             }
  #           ''
  #         else
  #           ""
  #       }
  #     '';
  #     runAlways = true;
  #     deps = [ "brave0" ];
  #   };
  # };

  # home.activation = lib.mkIf lib.profiles.isSystemWsl {
  #   oh-my-posh0 = lib.common.runExternalHome "oh-my-posh0" (
  #     lib.common.winGetInstallExternal "JanDeDobbeleer.OhMyPosh" { }
  #   );
  #   oh-my-posh1 = lib.common.runExternalHome "oh-my-posh1" {
  #     text = ''
  #       ${lib.getExe' pkgs.coreutils "install"} -D ${env.user.configDirectory}/oh-my-posh/config.json ${
  #         lib.escape [ " " ] env.extra.winUser.configDirectory
  #       }/oh-my-posh
  #     '';
  #     runAlways = true;
  #     deps = [ "oh-my-posh0" ];
  #   };
  # };

  # home.activation = {
  #   ssh0 = lib.common.runExternalHome "ssh0" {
  #     text = ''
  #       test -f ${env.user.homeDirectory}/.ssh/id_ed25519 || ${lib.getExe' pkgs.openssh "ssh-keygen"} -t ed25519 -a 100 -N "" -f ${env.user.homeDirectory}/.ssh/id_ed25519
  #       # test -f ${env.user.homeDirectory}/.ssh/id_rsa || ${lib.getExe' pkgs.openssh "ssh-keygen"} -t rsa -b 4096 -o -a 100 -N "" -f ${env.user.homeDirectory}/.ssh/id_rsa
  #     '';
  #   };
  #   ssh1 = lib.mkIf lib.profiles.isSystemWsl (
  #     lib.common.runExternalHome "ssh1" {
  #       text = ''
  #         ${lib.getExe' pkgs.coreutils "install"} -D ${env.user.homeDirectory}/.ssh/config ${
  #           lib.escape [ " " ] env.extra.winUser.homeDirectory
  #         }/.ssh
  #       '';
  #       runAlways = true;
  #     }
  #   );
  # };

  # home.activation = lib.mkIf lib.profiles.isSystemWsl {
  #   starship0 = lib.common.runExternalHome "starship0" (
  #     lib.common.winGetInstallExternal "Starship.Starship" { }
  #   );
  #   starship1 = lib.common.runExternalHome "starship1" {
  #     text = ''
  #       ${lib.getExe' pkgs.coreutils "install"} -D ${env.user.configDirectory}/starship.toml ${
  #         lib.escape [ " " ] env.extra.winUser.configDirectory
  #       }
  #     '';
  #     runAlways = true;
  #     deps = [ "starship0" ];
  #   };
  # };

  # home.activation.wiztree0 = lib.mkIf lib.profiles.isSystemWsl (
  #   lib.common.runExternalHome "wiztree0" (
  #     lib.common.winGetInstallExternal "AntibodySoftware.WizTree" { }
  #   )
  # );

  # system.activationScripts = {
  #   chocolatey0 = lib.common.runExternalSystem "chocolatey0" {
  #     text = ''
  #       powershell.exe -Command "& {Set-ExecutionPolicy Bypass -Scope Process -Force; [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; iex ((New-Object System.Net.WebClient).DownloadString('https://community.chocolatey.org/install.ps1'))}"
  #     '';
  #   };
  #   chocolatey1 = lib.common.runExternalSystem "chocolatey1" {
  #     text = ''
  #       choco.exe upgrade all -y
  #     '';
  #     runAlways = true;
  #     deps = [ "chocolatey0" ];
  #   };
  # };

  # system.activationScripts = {
  #   glazewm0 = lib.common.runExternalSystem "glazewm0" (
  #     lib.common.winGetInstallExternal "lars-berger.GlazeWM" { }
  #   );
  #   glazewm1 = lib.common.runExternalSystem "glazewm1" {
  #     text = ''
  #       ${lib.getExe' pkgs.coreutils "install"} -DT ${./glazewm/config.yaml} ${
  #         lib.escape [ " " ] env.extra.winUser.userProfile
  #       }/.glzr/glazewm/config.yaml
  #     '';
  #     runAlways = true;
  #     deps = [ "glazewm0" ];
  #   };
  # };

  # system.activationScripts = {
  #   winget0 = lib.common.runExternalSystem "winget0" {
  #     text = ''
  #       winget.exe update --all --uninstall-previous --disable-interactivity
  #     '';
  #     runAlways = true;
  #   };
  # };

  home.stateVersion = "22.05";
}
