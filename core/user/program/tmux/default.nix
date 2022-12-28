{
  config,
  lib,
  pkgs,
  ...
}: {
  clock24 = true;
  keyMode = "vi";
  plugins = with pkgs.tmuxPlugins; [
    {
      plugin = continuum;
      extraConfig = ''
        set -g @continuum-restore 'on'
        set -g @continuum-save-interval '60' # minutes
      '';
    }
    {
      plugin = cpu;
      extraConfig = ''
        set -g status-justify 'centre'
        set -g status-left-length '100'
        set -g status-left '[#S] #[fg=black,bold]#W#F#[default] #{cpu_fg_color}#{cpu_percentage}#[default] #{ram_fg_color}#{ram_percentage}#[default] #{cpu_temp_fg_color}#{cpu_temp}#[default]'
      '';
    }
    {
      plugin = prefix-highlight;
      extraConfig = ''
        set -g @prefix_highlight_fg 'white'
        set -g @prefix_highlight_bg 'blue,bold'
        set -g @prefix_highlight_copy_prompt 'COPY'
        set -g @prefix_highlight_sync_prompt 'SYNC'
        set -g @prefix_highlight_show_copy_mode 'on'
        set -g @prefix_highlight_copy_mode_attr 'fg=white,bg=blue,bold'
        set -g @prefix_highlight_show_sync_mode 'on'
        set -g @prefix_highlight_sync_mode_attr 'fg=white,bg=blue,bold'

        set -g status-right-length '100'
        set -g status-right '#{prefix_highlight} #[fg=cyan,bold]#U#[default]#[fg=black,bold]@#[default]#[fg=green,bold]#{?#{pane_ssh_connected},<ssh>,}#[default]#[fg=black,bold]#H#[default] %y-%m-%d %R'
      '';
    }
    {
      plugin = resurrect;
      extraConfig = "set -g @resurrect-strategy-nvim 'session'";
    }
    {
      plugin =
        mkTmuxPlugin
        rec {
          pluginName = "base16-tmux";
          rtpFilePath = "tmuxcolors.tmux";
          version = "unstable";
          src = pkgs.fetchFromGitHub {
            owner = "tinted-theming";
            repo = pluginName;
            rev = "c3366618c6d746647051b3cd0ded23f3f7c332e1";
            hash = "sha256-lbe/Ov+ShmlA4O/qAYX8K9ZSKvCzekyXKMcSpzfyXvA=";
          };
        };
      extraConfig = "set -g @colors-base16 'chalk'";
    }
    {
      plugin =
        mkTmuxPlugin
        rec {
          pluginName = "tmux-current-pane-hostname";
          rtpFilePath = "current_pane_hostname.tmux";
          version = "unstable";
          src = pkgs.fetchFromGitHub {
            owner = "soyuka";
            repo = pluginName;
            rev = "6bb3c95250f8120d8b072f46a807d2678ecbc97c";
            hash = "sha256-vmGdHAWpYwo95tJNZlu9M5ZaC0qazTP4vT7tUAZHPfA=";
          };
        };
    }
  ];
  prefix = "C-a";
  tmuxp.enable = true;
}
