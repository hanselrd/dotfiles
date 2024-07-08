{
  config,
  lib,
  pkgs,
  env,
  ...
}: let
  cfg = config.roles.user.oh-my-posh;
in {
  options = {
    roles.user.oh-my-posh = {
      enable = lib.mkEnableOption "roles.user.oh-my-posh";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.oh-my-posh = {
      enable = true;
      enableBashIntegration = true;
      enableZshIntegration = true;
      settings = {
        disable_notice = true;
        final_space = true;
        version = 2;
        blocks = [
          {
            type = "prompt";
            alignment = "left";
            segments = [
              {
                type = "time";
                style = "plain";
                foreground = "lightYellow";
                template = lib.concatStrings [
                  "<b>"
                  "{{.CurrentDate | date .Format}}"
                  "</b> "
                ];
                properties = {
                  time_format = env.extra.goTimeFormat;
                };
              }
              {
                type = "session";
                style = "plain";
                foreground = "lightCyan";
                foreground_templates = [
                  "{{if .Root}}lightRed{{end}}"
                ];
                template = lib.concatStrings [
                  "<b>"
                  "{{.UserName}}"
                  "</b>"
                ];
              }
              {
                type = "session";
                style = "plain";
                foreground = "darkGray";
                foreground_templates = [
                  "{{if .SSHSession}}lightGreen{{end}}"
                ];
                template = lib.concatStrings [
                  "<b>"
                  "<darkGray>@</>"
                  "{{.HostName}}"
                  # "{{if .Env.TMUX}}"
                  # "<darkGray>|</>"
                  # "<lightYellow>tmux</>"
                  # "{{end}}"
                  "</b> "
                ];
              }
              {
                type = "path";
                style = "plain";
                foreground = "lightCyan";
                foreground_templates = [
                  "{{if not .Writable}}lightRed{{end}}"
                ];
                template = lib.concatStrings [
                  "<b>"
                  "{{.Path}}"
                  "<darkGray>|</>"
                  "<lightYellow>{{.StackCount}}</>"
                  "</b> "
                ];
                properties = {
                  style = "folder";
                };
              }
              {
                type = "git";
                style = "plain";
                foreground = "lightMagenta";
                template = lib.concatStrings [
                  "<b>"
                  "{{.HEAD}}"
                  "{{if or .BranchStatus .Staging.Changed .Working.Changed}}<darkGray>|</>{{end}}"
                  "{{if .BranchStatus}}<lightRed>{{.BranchStatus}}</>{{end}}"
                  "{{if .Staging.Changed}}<lightRed>{{nospace .Staging.String}}</>{{end}}"
                  "{{if .Working.Changed}}<lightRed>{{nospace .Working.String}}</>{{end}}"
                  "<darkGray>|</>"
                  "<lightGreen>{{trunc 6 .Commit.Sha}}</>"
                  "<darkGray>|</>"
                  "<lightYellow>{{.StashCount}}</>"
                  "</b>"
                ];
                properties = {
                  fetch_status = true;
                  # fetch_bare_info = true;
                  branch_icon = "";
                  branch_identical_icon = "=";
                  branch_ahead_icon = "^";
                  branch_behind_icon = "v";
                  branch_gone_icon = "=/";
                  truncate_symbol = "..";
                  commit_icon = "";
                  tag_icon = "";
                  rebase_icon = "";
                  cherry_pick_icon = "";
                  revert_icon = "";
                  merge_icon = "";
                  no_commits_icon = "";
                };
              }
            ];
          }
          {
            type = "prompt";
            alignment = "right";
            segments = [
              {
                type = "executiontime";
                style = "plain";
                foreground = "darkGray";
                template = lib.concatStrings [
                  "<b>"
                  "{{.FormattedMs}}"
                  "</b>"
                ];
                properties = {
                  always_enabled = true;
                  style = "austin";
                };
              }
              {
                type = "command";
                style = "plain";
                foreground = "lightRed";
                template = lib.concatStrings [
                  "<b>"
                  "<darkGray>|</>"
                  "<lightRed>{{.Output}}</>"
                  "</b>"
                ];
                properties = {
                  command = "[ -f /var/lock/prevent_idle_terminate ] && ${lib.getExe' pkgs.coreutils "echo"} \"lock\"";
                  interpret = false;
                };
              }
              {
                type = "command";
                style = "plain";
                foreground = "lightRed";
                template = lib.concatStrings [
                  "<b>"
                  "<darkGray>|</>"
                  "<lightRed>idle:{{.Output}}</>"
                  "</b>"
                ];
                properties = {
                  command = "${lib.getExe' pkgs.coreutils "cat"} /etc/idle_terminate_threshold";
                };
              }
              {
                type = "text";
                style = "plain";
                foreground = "lightYellow";
                foreground_templates = [
                  "{{if not (eq .Code 0)}}lightRed{{end}}"
                ];
                template = lib.concatStrings [
                  "<b>"
                  "<darkGray>|</>"
                  "{{.Code}}"
                  "</b>"
                ];
              }
            ];
          }
          {
            type = "prompt";
            alignment = "left";
            newline = true;
            segments = [
              {
                type = "shell";
                style = "plain";
                foreground = "darkGray";
                template = lib.concatStrings [
                  "<b>"
                  "{{.Name}}"
                  "|"
                  "<lightYellow>{{.SHLVL}}</>"
                  "</b> "
                ];
              }
              {
                type = "text";
                style = "plain";
                foreground = "lightCyan";
                foreground_templates = [
                  "{{if not (eq .Code 0)}}lightRed{{end}}"
                ];
                template = lib.concatStrings [
                  "<b>"
                  "{{\">\"}}"
                  "</b>"
                ];
              }
            ];
          }
        ];
        # transient_prompt = {
        #   foreground = "lightCyan";
        #   foreground_templates = [
        #     "{{if not (eq .Code 0)}}lightRed{{end}}"
        #   ];
        #   template = lib.concatStrings [
        #     "<b>"
        #     "<darkGray>{{.Shell}}</>"
        #     "<darkGray>|</>"
        #     "<lightYellow>{{.SHLVL}}</> "
        #     "{{\">\"}}"
        #     "</b> "
        #   ];
        # };
      };
    };
  };
}
