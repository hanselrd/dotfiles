{
  config,
  lib,
  pkgs,
  secretsPath,
  env,
  ...
}:
{
  environment.systemPackages = with pkgs; [ cifs-utils ];

  fileSystems."/data" = {
    device = "//10.10.0.100/data";
    fsType = "cifs";
    options = [
      "credentials=${lib.x.decryptSecret env.identity (secretsPath + "/smb-data-creds.age")}"
      "uid=${builtins.toString config.users.users.${env.username}.uid}"
      "gid=${builtins.toString config.users.groups.${config.users.users.${env.username}.group}.gid}"
      "x-systemd.automount"
      "noauto"
      "x-systemd.device-timeout=5"
      "x-systemd.idle-timeout=60"
      "x-systemd.mount-timeout=5"
      "dir_mode=0755"
      "file_mode=0644"
    ];
  };
}
