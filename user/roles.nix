{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./roles/browser.nix
    ./roles/development.nix
    ./roles/docker.nix
    ./roles/editor.nix
    ./roles/homeage.nix
    ./roles/nix.nix
    ./roles/pager.nix
    ./roles/shell.nix
    ./roles/terminal.nix
    ./roles/theme.nix
  ];
}
