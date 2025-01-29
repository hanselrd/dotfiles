{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.roles.system.chocolatey;
in
{
  options = {
    roles.system.chocolatey = {
      enable = lib.mkEnableOption "roles.system.chocolatey";
    };
  };

  config = lib.mkIf cfg.enable (
    lib.optionalAttrs lib.profiles.isSystemWsl {
      system.activationScripts = {
        chocolatey0 = lib.common.runExternalSystem "chocolatey0" {
          text = ''
            powershell.exe -Command "& {Set-ExecutionPolicy Bypass -Scope Process -Force; [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; iex ((New-Object System.Net.WebClient).DownloadString('https://community.chocolatey.org/install.ps1'))}"
          '';
        };
        chocolatey1 = lib.common.runExternalSystem "chocolatey1" {
          text = ''
            choco.exe upgrade all -y
          '';
          runAlways = true;
          deps = [ "chocolatey0" ];
        };
      };
    }
  );
}
