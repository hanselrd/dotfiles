{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./generic.nix
  ];

  roles.system.wsl.enable = true;
}
