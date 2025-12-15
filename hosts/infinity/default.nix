{ nixosModulesPath, ... }:
{
  imports = [
    ./hardware-configuration.nix
    (nixosModulesPath + "/common")
    (nixosModulesPath + "/services/qemu-guest")
  ];

  system.stateVersion = "24.11";
}
