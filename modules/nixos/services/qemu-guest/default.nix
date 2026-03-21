{ pkgs, ... }:
{
  services.qemuGuest.enable = true;

  environment.systemPackages = with pkgs; [ cloud-utils ];
}
