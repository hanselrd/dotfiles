{
  config,
  lib,
  pkgs,
  ...
}: {
  settings = {
    format = lib.concatStrings [
      "\${custom.lock}"
      "\${custom.idle}"
      "$time"
      "$sudo"
      "$username"
      "$hostname"
      "$directory"
      "$git_branch"
      "$git_commit"
      "$cmd_duration"
      "$line_break"
      "$shell"
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
      format = "[$branch(:$remote_branch)]($style)";
    };
    git_commit = {
      format = "[|](bold bright-black)[$hash]($style) ";
      commit_hash_length = 6;
      only_detached = false;
    };
    hostname = {
      format = "[@](bold bright-black)[$ssh_symbol](bold green)[$hostname]($style) ";
      ssh_only = false;
      ssh_symbol = "<ssh>";
      style = "bold bright-black";
    };
    shell = {
      format = "[\\[$indicator\\]]($style) ";
      style = "bold bright-black";
      disabled = false;
    };
    status = {
      symbol = "x";
      disabled = false;
    };
    sudo = {
      format = "[$symbol]($style)";
      symbol = "<sudo>";
      style = "bold red";
      disabled = false;
    };
    time = {
      format = "[{$time}]($style) ";
      time_format = "%y-%m-%d %R";
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
      command = "cat /etc/idle_terminate_threshold";
      when = "[ -f /etc/idle_terminate_threshold ]";
      symbol = "idle";
      style = "bold red";
      os = "linux";
    };
  };
}
