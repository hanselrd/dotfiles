{
  config,
  lib,
  pkgs,
  ...
}: {
  roles.user.bootstrap.enable = true;
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
    cpio
    curl
    git-crypt
    pfetch
    rsync
    strace
    tree
    tshark
    wget
  ];
}
