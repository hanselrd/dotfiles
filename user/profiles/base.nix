{
  config,
  lib,
  pkgs,
  env,
  ...
}:
(lib.recursiveUpdate {
    roles.user.bootstrap.enable = true;
    roles.user.editor.enable = true;
    roles.user.nix.enable = true;
    roles.user.pager.enable = true;
    roles.user.ripgrep.enable = true;
    roles.user.scripts.enable = true;
    roles.user.shell.enable = true;
    roles.user.theme.enable = true;
    roles.user.zzz.enable = true;
  }
  (lib.optionalAttrs (!env.extra.encrypted) {
    # roles.user.rts.enable = true;
  }))
// {
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
