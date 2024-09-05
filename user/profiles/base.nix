{
  config,
  lib,
  pkgs,
  env,
  ...
}: {
  roles.user.bootstrap.enable = true;
  roles.user.scripts.enable = true;
  roles.user.editor.enable = true;
  # roles.user.rts.enable = lib.mkIf (!env.extra.encrypted) true;
  roles.user.nix.enable = true;
  roles.user.pager.enable = true;
  roles.user.ripgrep.enable = true;
  roles.user.shell.enable = true;
  roles.user.theme.enable = true;
  roles.user.zzz.enable = true;

  home.packages = with pkgs; [
    age
    coreutils
    cpio
    curl
    dmidecode
    file
    findutils
    gnugrep
    gnused
    gzip
    lm_sensors
    lsb-release
    pfetch
    procps
    rsync
    ssh-to-age
    strace
    sysstat
    tree
    wget
  ];
}
