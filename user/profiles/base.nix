{
  config,
  lib,
  pkgs,
  env,
  ...
}:
(lib.recursiveUpdate
  {
    roles.user.bootstrap.enable = true;
    roles.user.bat.enable = true;
    roles.user.bin.enable = true;
    roles.user.btop.enable = true;
    roles.user.editor.enable = true;
    roles.user.eza.enable = true;
    roles.user.fastfetch.enable = true;
    roles.user.fzf.enable = true;
    roles.user.git.enable = true;
    roles.user.htop.enable = true;
    roles.user.nix.enable = true;
    roles.user.pager.enable = true;
    roles.user.ripgrep.enable = true;
    roles.user.shell.enable = true;
    roles.user.ssh.enable = true;
    roles.user.theme.enable = true;
    roles.user.tmux.enable = true;
    roles.user.xdg.enable = true;
    roles.user.zoxide.enable = true;
  }
  (
    lib.optionalAttrs (!env.extra.encrypted.yellow) {
      # roles.user.rts.enable = true;
    }
  )
)
// {
  home.packages = with pkgs; [
    age
    coreutils
    cpio
    curl
    diffutils
    dmidecode
    file
    findutils
    gawk
    gnugrep
    gnused
    gnutar
    gzip
    hexxy
    killall
    lm_sensors
    lsb-release
    miller
    pfetch
    pqrs
    procps
    rsync
    ssh-to-age
    strace
    sysstat
    tree
    unzip
    wget
    xxd
    zstd
  ];
}
