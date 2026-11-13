{
  modules.nixos.scx-loader-service = {
    services.scx-loader = {
      enable = true;
      config = {
        default_sched = "scx_lavd";
        default_mode = "Auto";
        scheds.scx_lavd = {
          auto_mode = [
            "--autopilot"
            "--pinned-slice-us"
            "500"
          ];
          gaming_mode = [
            "--performance"
            "--pinned-slice-us"
            "500"
          ];
          lowlatency_mode = [
            "--performance"
            "--pinned-slice-us"
            "500"
          ];
          powersave_mode = [
            "--powersave"
            "--pinned-slice-us"
            "500"
          ];
          server_mode = [
            "--performance"
            "--slice-min-us"
            "3000"
            "--slice-max-us"
            "10000"
            "--pinned-slice-us"
            "3000"
          ];
        };
      };
    };
  };
}
