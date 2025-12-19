{
  config,
  lib,
  pkgs,
  env,
  ...
}:
{
  programs.starship = {
    enable = true;
    settings = {
      format = lib.concatStrings [
        "\${custom.lock}"
        "\${custom.idle}"
        "$nix_shell"
        "$time"
        "$sudo"
        "$username"
        "$hostname"
        "$directory"
        "$git_branch"
        "$git_status"
        "$git_commit"
        "$cmd_duration"
        "$line_break"
        "$shell"
        "$shlvl"
        "$status"
        "$character"
      ];
      scan_timeout = 10;
      add_newline = false;
      character = {
        success_symbol = "[>](bold cyan)";
        error_symbol = "[x](bold red)";
        vimcmd_symbol = "[<](bold cyan)";
      };
      cmd_duration = {
        format = "[<$duration>]($style) ";
        style = "bold bright-black";
      };
      directory = {
        format = "[$read_only]($read_only_style)[$path]($style) ";
        read_only = "<ro>";
        read_only_style = "bold red";
      };
      git_branch = {
        format = "[$branch]($style)[(:$remote_branch)](bold bright-black)";
      };
      git_commit = {
        format = "[|](bold bright-black)[$hash]($style) ";
        commit_hash_length = 6;
        only_detached = false;
      };
      git_status = {
        format = "([\\($all_status$ahead_behind\\)]($style))";
        conflicted = "=[$count](bold bright-black)";
        ahead = "^[$count](bold bright-black)";
        behind = "v[$count](bold bright-black)";
        diverged = "@[(+$ahead_count)(-$behind_count)](bold bright-black)";
        up_to_date = "";
        untracked = "?[$count](bold bright-black)";
        stashed = "\\$[$count](bold bright-black)";
        modified = "![$count](bold bright-black)";
        staged = "+[$count](bold bright-black)";
        renamed = ">>[$count](bold bright-black)";
        deleted = "x[$count](bold bright-black)";
        style = "bold purple";
        # windows_starship = "/mnt/c/ProgramData/chocolatey/bin/starship.exe";
        windows_starship = "/mnt/c/Program Files/starship/bin/starship.exe";
      };
      hostname = {
        format = "[@](bold bright-black)$ssh_symbol[$hostname]($style) ";
        ssh_only = false;
        ssh_symbol = "[<ssh>](bold green)";
        style = "bold bright-black";
      };
      nix_shell = {
        format = "[<$symbol]($style)[@](bold bright-black)$state[(:$name)](bold bright-black)[>]($style) ";
        symbol = "nix";
        impure_msg = "[impure](bold red)";
        pure_msg = "[pure](bold green)";
        style = "bold purple";
      };
      shell = {
        format = "[$indicator]($style)";
        style = "bold bright-black";
        disabled = false;
      };
      shlvl = {
        format = "[|](bold bright-black)[$shlvl]($style) ";
        threshold = 2;
        style = "bold yellow";
        disabled = false;
      };
      status = {
        symbol = "x";
        disabled = false;
      };
      sudo = {
        format = "[$symbol]($style)";
        symbol = "<su>";
        style = "bold red";
        # disabled = false;
      };
      time = {
        format = "[{$time}]($style) ";
        time_format = env.timeFormat;
        disabled = false;
      };
      username = {
        format = "[$user]($style)";
        show_always = true;
        style_user = "bold cyan";
        disabled = false;
      };
      custom.lock = {
        format = "[$symbol]($style) ";
        when = "[ -f /var/lock/prevent_idle_terminate ]";
        symbol = "<lock>";
        style = "bold red";
        os = "linux";
      };
      custom.idle = {
        format = "[<$symbol]($style)[@$output](bold bright-black)[>]($style) ";
        command = "${lib.getExe' pkgs.coreutils "cat"} /etc/idle_terminate_threshold";
        when = "[ -f /etc/idle_terminate_threshold ]";
        symbol = "idle";
        style = "bold red";
        os = "linux";
      };
    };
  };
}
