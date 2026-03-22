{ nixosModulesPath, ... }:
{
  imports = [
    ./hardware-configuration.nix
    (nixosModulesPath + "/common")
    (nixosModulesPath + "/services/qemu-guest")
    (nixosModulesPath + "/samba-shares")
  ];

  system.stateVersion = "25.11";
}
