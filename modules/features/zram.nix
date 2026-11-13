{
  modules.nixos.zram = {
    zramSwap = {
      enable = true;
      algorithm = "zstd";
      memoryPercent = 100;
      memoryMax = 8192 * 1024 * 1024;
    };

    boot.kernel.sysctl = {
      "vm.swappiness" = 180;
      "vm.page-cluster" = 0;
      "vm.vfs_cache_pressure" = 50;
      "vm.watermark_boost_factor" = 0;
      "vm.watermark_scale_factor" = 150;
    };
  };
}
