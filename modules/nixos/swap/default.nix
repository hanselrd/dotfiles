{ ... }:
{
  swapDevices = [
    {
      device = "/swapfile";
      size = 4 * 1024;
    }
  ];

  zramSwap.enable = true;
}
