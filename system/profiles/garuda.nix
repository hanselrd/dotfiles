{
  config,
  lib,
  pkgs,
  ...
}:
{
  imports = [
    ./nixos.nix
  ];

  roles.system.garuda.enable = true;
}
