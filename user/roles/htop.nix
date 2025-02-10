{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.roles.user.htop;
in
{
  options = {
    roles.user.htop = {
      enable = lib.mkEnableOption "roles.user.htop";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      htop
    ];

    xdg.configFile."htop/htoprc" = {
      text = ''
        fields=0 48 17 18 38 39 40 2 46 47 49 1
        hide_kernel_threads=1
        hide_userland_threads=0
        hide_running_in_container=0
        shadow_other_users=0
        show_thread_names=0
        show_program_path=1
        highlight_base_name=0
        highlight_deleted_exe=1
        shadow_distribution_path_prefix=0
        highlight_megabytes=1
        highlight_threads=1
        highlight_changes=0
        highlight_changes_delay_secs=5
        find_comm_in_cmdline=1
        strip_exe_from_cmdline=1
        show_merged_command=0
        header_margin=1
        screen_tabs=1
        detailed_cpu_time=0
        cpu_count_from_one=0
        show_cpu_usage=1
        show_cpu_frequency=0
        show_cpu_temperature=0
        degree_fahrenheit=0
        update_process_names=0
        account_guest_in_cpu_meter=0
        color_scheme=0
        enable_mouse=1
        delay=15
        hide_function_bar=0
        header_layout=two_50_50
        column_meters_0=AllCPUs Memory Swap
        column_meter_modes_0=1 1 1
        column_meters_1=Tasks LoadAverage Uptime
        column_meter_modes_1=2 2 2
        tree_view=0
        sort_key=46
        tree_sort_key=0
        sort_direction=-1
        tree_sort_direction=1
        tree_view_always_by_pid=0
        all_branches_collapsed=0
        screen:Main=PID USER PRIORITY NICE M_VIRT M_RESIDENT M_SHARE STATE PERCENT_CPU PERCENT_MEM TIME Command
        .sort_key=PERCENT_CPU
        .tree_sort_key=PID
        .tree_view_always_by_pid=0
        .tree_view=0
        .sort_direction=-1
        .tree_sort_direction=1
        .all_branches_collapsed=0
        screen:I/O=PID USER IO_PRIORITY IO_RATE IO_READ_RATE IO_WRITE_RATE PERCENT_SWAP_DELAY PERCENT_IO_DELAY Command
        .sort_key=IO_RATE
        .tree_sort_key=PID
        .tree_view_always_by_pid=0
        .tree_view=0
        .sort_direction=-1
        .tree_sort_direction=1
        .all_branches_collapsed=0
      '';
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
  };
}
