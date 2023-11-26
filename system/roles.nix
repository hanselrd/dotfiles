{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./roles/boot.nix
    ./roles/bootstrap.nix
    ./roles/i18n.nix
    ./roles/monitoring.nix
    ./roles/motd.nix
    ./roles/networking.nix
    ./roles/nix.nix
    ./roles/shell.nix
    ./roles/time.nix
    ./roles/user.nix
    ./roles/virtualization.nix
    ./roles/x11.nix
  ];
}
