{
  config,
  lib,
  pkgs,
  ...
}: {
  roles.user.bootstrap.enable = true;
  roles.user.scripts.enable = true;
  roles.user.editor.enable = true;
  roles.user.homeage.enable = true;
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
    pfetch
    rsync
    ssh-to-age
    strace
    tree
    wget
  ];
}
