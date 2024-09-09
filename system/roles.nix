{
  config,
  lib,
  pkgs,
  env,
  ...
}: {
  imports = lib.flatten [
    ./roles/bootstrap.nix
    ./roles/boot.nix
    ./roles/garuda.nix
    ./roles/home-manager.nix
    ./roles/i18n.nix
    ./roles/kernel.nix
    ./roles/monitoring.nix
    ./roles/motd.nix
    ./roles/networking.nix
    ./roles/nix.nix
    ./roles/openssh.nix
    ./roles/shell.nix
    ./roles/sudo.nix
    ./roles/time.nix
    ./roles/user.nix
    ./roles/virtualization.nix
    ./roles/wsl.nix
    ./roles/x11.nix
  ];
}
