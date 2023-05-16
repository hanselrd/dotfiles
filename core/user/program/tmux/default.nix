{
  config,
  lib,
  pkgs,
  ...
}: {
  clock24 = true;
  keyMode = "vi";
  plugins = with pkgs.tmuxPlugins; [
    better-mouse-mode
    extrakto
    pain-control
    yank
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
    # {
    #   plugin =
    #     mkTmuxPlugin
    #     rec {
    #       pluginName = "base16-tmux";
    #       rtpFilePath = "tmuxcolors.tmux";
    #       version = "unstable";
    #       src = pkgs.fetchFromGitHub {
    #         owner = "tinted-theming";
    #         repo = pluginName;
    #         rev = "3312bb2cbb26db7eeb2d2235ae17d4ffaef5e59b";
    #         hash = "sha256-HcJwx1tucRP0XKkEIaP831xJCW8wwKvbiksDl9r9zNw=";
    #       };
    #     };
    #   extraConfig = "set -g @colors-base16 'chalk'";
    # }
  ];
  extraConfig = with config.colorScheme.colors; ''
    # default statusbar colors
    set-option -g status-style "fg=#${base04},bg=#${base01}"

    # default window title colors
    set-window-option -g window-status-style "fg=#${base04},bg=default"

    # active window title colors
    set-window-option -g window-status-current-style "fg=#${base0A},bg=default"

    # pane border
    set-option -g pane-border-style "fg=#${base01}"
    set-option -g pane-active-border-style "fg=#${base02}"

    # message text
    set-option -g message-style "fg=#${base05},bg=#${base01}"

    # pane number display
    set-option -g display-panes-active-colour "#${base0B}"
    set-option -g display-panes-colour "#${base0A}"

    # clock
    set-window-option -g clock-mode-colour "#${base0B}"

    # copy mode highligh
    set-window-option -g mode-style "fg=#${base04},bg=#${base02}"

    # bell
    set-window-option -g window-status-bell-style "fg=#${base01},bg=#${base08}"
  '';
  prefix = "C-a";
  tmuxp.enable = true;
}
