{
  config,
  lib,
  pkgs,
  env,
  ...
}:
{
  programs.oh-my-posh = {
    enable = true;
    enableBashIntegration = config.programs.bash.enable;
    enableZshIntegration = config.programs.zsh.enable;
    settings = with config.lib.stylix.colors.withHashtag; {
      disable_notice = true;
      final_space = true;
      version = 2;
      blocks = [
        {
          type = "prompt";
          alignment = "left";
          segments = [
            {
              type = "command";
              style = "plain";
              foreground = bright-red;
              template = lib.concatStrings [
                "<b>"
                "{{.Output}}"
                "</b> "
              ];
              properties = {
                command = "[ -f /var/lock/prevent_idle_terminate ] && ${lib.getExe' pkgs.coreutils "echo"} \"lock\"";
                interpret = false;
              };
            }
            {
              type = "command";
              style = "plain";
              foreground = bright-red;
              template = lib.concatStrings [
                "<b>"
                "idle("
                "<darkGray>{{.Output}}</>"
                ")"
                "</b> "
              ];
              properties = {
                command = "${lib.getExe' pkgs.coreutils "cat"} /etc/idle_terminate_threshold";
              };
            }
            {
              type = "nix-shell";
              style = "plain";
              foreground = bright-magenta;
              template = lib.concatStrings [
                "<b>"
                "$nix("
                "<darkGray>{{.Type}}</>"
                ")"
                "</b> "
              ];
            }
            {
              # type = "time";
              type = "command";
              style = "plain";
              foreground = bright-yellow;
              template = lib.concatStrings [
                "<b>"
                "<darkGray>{</>"
                # "{{.CurrentDate | date .Format}}"
                "{{.Output}}"
                "<darkGray>}</>"
                "</b> "
              ];
              properties = {
                # time_format =
                command = "${lib.getExe' pkgs.coreutils "date"} +\"${
                  lib.replaceStrings
                    [ "<" ">" ]
                    [
                      (lib.concatStrings [
                        "<darkGray><</>"
                        "<${bright-red}>"
                      ])
                      (lib.concatStrings [
                        "</>"
                        "<darkGray>></>"
                      ])
                    ]
                    # env.goTimeFormat;
                    env.timeFormat
                }\"";
              };
            }
            {
              type = "session";
              style = "plain";
              foreground = bright-cyan;
              foreground_templates = [ "{{if .Root}}${bright-red}{{end}}" ];
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
              foreground_templates = [ "{{if .SSHSession}}${bright-green}{{end}}" ];
              template = lib.concatStrings [
                "<b>"
                "<darkGray>@</>"
                "{{index (regexSplit `\\.` .HostName 2) 0}}"
                # "<darkGray>|</>"
                # "<${bright-blue}>{{.OS}}</>"
                "</b> "
              ];
            }
            {
              type = "path";
              style = "plain";
              foreground = bright-cyan;
              foreground_templates = [ "{{if not .Writable}}${bright-red}{{end}}" ];
              template = lib.concatStrings [
                "<b>"
                "{{.Path}}"
                "{{if gt .StackCount 0}}<darkGray>|</><${bright-yellow}>{{.StackCount}}</>{{end}}"
                "</b> "
              ];
              properties = {
                style = "folder";
              };
            }
            {
              type = "git";
              style = "plain";
              foreground = bright-magenta;
              template = lib.concatStrings [
                "<b>"
                "{{.HEAD}}"
                "{{if or .BranchStatus .Staging.Changed .Working.Changed}}<darkGray>|</>{{end}}"
                "{{if .BranchStatus}}<${bright-red}>{{.BranchStatus}}</>{{end}}"
                "{{if .Staging.Changed}}<${bright-red}>{{nospace .Staging.String}}</>{{end}}"
                "{{if .Working.Changed}}<${bright-red}>{{nospace .Working.String}}</>{{end}}"
                "<darkGray>|</>"
                "<${bright-green}>{{trunc 6 .Commit.Sha}}</>"
                "{{if gt .StashCount 0}}<darkGray>|</><${bright-yellow}>{{.StashCount}}</>{{end}}"
                "</b>"
              ];
              properties = {
                fetch_status = true;
                # fetch_bare_info = true;
                branch_icon = "";
                branch_identical_icon = "";
                branch_ahead_icon = "^";
                branch_behind_icon = "v";
                branch_gone_icon = "";
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
              type = "text";
              style = "plain";
              foreground = bright-yellow;
              foreground_templates = [ "{{if not (eq .Code 0)}}${bright-red}{{end}}" ];
              template = lib.concatStrings [
                "<b>"
                "{{if not (eq .Code 0)}}<darkGray>|</>{{.Code}}{{end}}"
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
                "{{if gt .SHLVL 1}}<darkGray>|</><${bright-yellow}>{{.SHLVL}}</>{{end}}"
                "</b> "
              ];
            }
            {
              type = "text";
              style = "plain";
              foreground = bright-cyan;
              foreground_templates = [ "{{if not (eq .Code 0)}}${bright-red}{{end}}" ];
              template = lib.concatStrings [
                "<b>"
                ">"
                "</b>"
              ];
            }
          ];
        }
      ];
      # transient_prompt = {
      #   foreground = bright-cyan;
      #   foreground_templates = [
      #     "{{if not (eq .Code 0)}}${bright-red}{{end}}"
      #   ];
      #   template = lib.concatStrings [
      #     "<b>"
      #     "<darkGray>{{.Shell}}</>"
      #     "{{if gt .SHLVL 1}}<darkGray>|</><${bright-yellow}>{{.SHLVL}}</>{{end}} "
      #     ">"
      #     "</b> "
      #   ];
      # };
      secondary_prompt = {
        foreground = bright-cyan;
        foreground_templates = [ "{{if not (eq .Code 0)}}${bright-red}{{end}}" ];
        template = lib.concatStrings [
          "<b>"
          "<darkGray>{{.Shell}}</>"
          "{{if gt .SHLVL 1}}<darkGray>|</><${bright-yellow}>{{.SHLVL}}</>{{end}} "
          ">>"
          "</b> "
        ];
      };
    };
  };
}
