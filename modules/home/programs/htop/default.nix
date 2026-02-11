{ lib, pkgs, ... }:
let
  formatOption =
    n: v:
    let
      v' = if lib.isBool v then (if v then "1" else "0") else builtins.toString v;
    in
    "${n}=${v'}";

  formatMeters = index: meters: {
    "column_meters_${builtins.toString index}" = lib.concatMap (lib.mapAttrsToList (x: _: x)) meters;
    "column_meter_modes_${builtins.toString index}" = lib.concatMap (lib.mapAttrsToList (
      _: y: y
    )) meters;
  };

  formatScreen = name: fields: {
    "screen:${name}" = [
      "PGRP"
      "PID"
      "USER"
      "PROCESSOR"
      "PRIORITY"
      "NICE"
      "STATE"
      "NLWP"
    ]
    ++ fields
    ++ [ "Command" ];
  };

  formatScreenSettings = sortKey: {
    ".all_branches_collapsed" = 0;
    ".sort_direction" = -1;
    ".sort_key" = sortKey;
    ".tree_sort_direction" = 1;
    ".tree_sort_key" = "PID";
    ".tree_view" = 0;
    ".tree_view_always_by_pid" = 0;
  };

  modes = {
    Bar = 1;
    Text = 2;
    Graph = 3;
    LED = 4;
  };

  meter = mode: name: { ${name} = mode; };
  bar = meter modes.Bar;
  text = meter modes.Text;
  # deadnix: skip
  graph = meter modes.Graph;
  # deadnix: skip
  led = meter modes.LED;
  # deadnix: skip
  blank = text "Blank";
in
{
  home.packages = with pkgs; [ htop ];

  xdg.configFile."htop/htoprc" = {
    text =
      [
        {
          config_reader_min_version = 3;
          header_layout = "two_50_50";
        }
        (formatMeters 0 [
          (bar "LeftCPUs2")
          (bar "CPU")
          (bar "Memory")
          (bar "Zram")
          (bar "Swap")
          (bar "HugePages")
        ])
        (formatMeters 1 [
          (bar "RightCPUs2")
          (text "System")
          (text "Tasks")
          (text "DiskIO")
          (text "NetworkIO")
          (text "LoadAverage")
          (text "Uptime")
        ])
        {
          account_guest_in_cpu_meter = 0;
          color_scheme = 0;
          cpu_count_from_one = 0;
          degree_fahrenheit = 0;
          delay = 15;
          detailed_cpu_time = 0;
          enable_mouse = 1;
          find_comm_in_cmdline = 1;
          header_margin = 0;
          hide_function_bar = 2;
          hide_kernel_threads = 1;
          hide_running_in_container = 0;
          hide_userland_threads = 0;
          highlight_base_name = 1;
          highlight_changes = 0;
          highlight_changes_delay_secs = 5;
          highlight_deleted_exe = 1;
          highlight_megabytes = 1;
          highlight_threads = 1;
          screen_tabs = 1;
          shadow_distribution_path_prefix = 1;
          shadow_other_users = 0;
          show_cpu_frequency = 1;
          show_cpu_temperature = 1;
          show_cpu_usage = 1;
          show_merged_command = 1;
          show_program_path = 1;
          show_thread_names = 1;
          strip_exe_from_cmdline = 1;
          update_process_names = 1;
        }
        (formatScreen "MAIN" [
          "PERCENT_CPU"
          "PERCENT_MEM"
          "TIME"
        ])
        (formatScreenSettings "PERCENT_CPU")
        (formatScreen "MAINX" [
          "CTXT"
          "PERCENT_CPU"
          "PERCENT_MEM"
          "TIME"
          "CCGROUP"
        ])
        (formatScreenSettings "PERCENT_CPU")
        (formatScreen "IO" [
          "IO_PRIORITY"
          "RCHAR"
          "WCHAR"
          "RBYTES"
          "WBYTES"
          "CNCLWB"
          "IO_RATE"
          "IO_READ_RATE"
          "IO_WRITE_RATE"
          "SYSCR"
          "SYSCW"
          "PERCENT_CPU_DELAY"
          "PERCENT_IO_DELAY"
          "PERCENT_SWAP_DELAY"
        ])
        (formatScreenSettings "IO_RATE")
        (formatScreen "MEM" [
          "M_VIRT"
          "M_RESIDENT"
          "M_SHARE"
          "M_TRS"
          "M_DRS"
          "M_LRS"
          "M_SWAP"
          "M_PSS"
          "M_PSSWP"
          "PERCENT_MEM"
          "OOM"
          "MINFLT"
          "CMINFLT"
          "MAJFLT"
          "CMAJFLT"
        ])
        (formatScreenSettings "PERCENT_MEM")
        (formatScreen "TIME" [
          "STARTTIME"
          "TIME"
          "STIME"
          "CSTIME"
          "UTIME"
          "CUTIME"
        ])
        (formatScreenSettings "TIME")
      ]
      |> (lib.concatMap (lib.mapAttrsToList formatOption))
      |> (lib.concatStringsSep "\n");
  };

  # programs.htop = {
  #   enable = true;
  #   settings =
  #     {
  #       account_guest_in_cpu_meter = 0;
  #       all_branches_collapsed = 0;
  #       color_scheme = 0;
  #       cpu_count_from_one = 0;
  #       degree_fahrenheit = 0;
  #       delay = 15;
  #       detailed_cpu_time = 0;
  #       enable_mouse = 1;
  #       fields = with config.lib.htop.fields; [
  #         PGRP
  #         PID
  #         USER
  #         PROCESSOR
  #         PRIORITY
  #         NICE
  #         STATE
  #         NLWP
  #         PERCENT_CPU
  #         PERCENT_MEM
  #         TIME
  #         COMM
  #       ];
  #       find_comm_in_cmdline = 1;
  #       header_margin = 1;
  #       hide_function_bar = 1;
  #       hide_kernel_threads = 1;
  #       hide_running_in_container = 0;
  #       hide_userland_threads = 0;
  #       highlight_base_name = 1;
  #       highlight_changes = 0;
  #       highlight_changes_delay_secs = 5;
  #       highlight_deleted_exe = 1;
  #       highlight_megabytes = 1;
  #       highlight_threads = 1;
  #       screen_tabs = 0;
  #       shadow_distribution_path_prefix = 1;
  #       shadow_other_users = 0;
  #       show_cpu_frequency = 1;
  #       show_cpu_temperature = 1;
  #       show_cpu_usage = 1;
  #       show_merged_command = 1;
  #       show_program_path = 1;
  #       show_thread_names = 1;
  #       sort_direction = -1;
  #       sort_key = config.lib.htop.fields.PERCENT_CPU;
  #       strip_exe_from_cmdline = 1;
  #       tree_sort_direction = 1;
  #       tree_sort_key = config.lib.htop.fields.PID;
  #       tree_view = 0;
  #       tree_view_always_by_pid = 0;
  #       update_process_names = 1;
  #     }
  #     // (
  #       with config.lib.htop;
  #       leftMeters [
  #         (bar "LeftCPUs2")
  #         (bar "CPU")
  #         (bar "Memory")
  #         (bar "Swap")
  #         (bar "HugePages")
  #       ]
  #     )
  #     // (
  #       with config.lib.htop;
  #       rightMeters [
  #         (bar "RightCPUs2")
  #         (text "System")
  #         (text "Tasks")
  #         (text "LoadAverage")
  #         (text "Uptime")
  #       ]
  #     );
  # };
}
