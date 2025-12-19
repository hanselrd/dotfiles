{
  config,
  lib,
  pkgs,
  env,
  ...
}:
{
  programs.tmux = {
    enable = true;
    clock24 = true;
    customPaneNavigationAndResize = true;
    historyLimit = 50000;
    keyMode = "vi";
    # mouse = true;
    newSession = true;
    terminal = "screen-256color";
    plugins = with pkgs.tmuxPlugins; [
      # battery
      # better-mouse-mode
      # cpu
      # extrakto
      # yank
      pain-control
      {
        plugin = mode-indicator.overrideAttrs (attrs: rec {
          version = "master";
          src = pkgs.fetchFromGitHub {
            owner = "MunifTanjim";
            repo = "tmux-${attrs.pluginName}";
            rev = version;
            hash = "sha256-SAzsn4LoG8Ju5t13/U3/ctlJQPyPgv2FjpPkWSeKbP0=";
          };
        });
        extraConfig = with config.lib.stylix.colors.withHashtag; ''
          set -g @mode_indicator_prefix_mode_style "fg=${base01},bg=${bright-red}"
          set -g @mode_indicator_prefix_prompt " PRFX "
          set -g @mode_indicator_copy_mode_style "fg=${base01},bg=${bright-yellow}"
          set -g @mode_indicator_copy_prompt " COPY "
          set -g @mode_indicator_sync_mode_style "fg=${base01},bg=${bright-cyan}"
          set -g @mode_indicator_sync_prompt " SYNC "
          set -g @mode_indicator_empty_mode_style "fg=${base01},bg=${bright-blue}"
          set -g @mode_indicator_empty_prompt " TMUX "

          set -g status-left-length 100
          set -g status-left ""
          set -ga status-left "#[fg=brightblack][#[default]"
          set -ga status-left "#{?client_prefix,#[fg=${bright-red}],#{?pane_in_mode,#[fg=${bright-yellow}],#{?pane_synchronized,#[fg=${bright-cyan}],#[fg=${bright-blue}]}}}#S#[default]"
          set -ga status-left "#[fg=brightblack]]#[default] "
          set -ga status-left "#[fg=brightmagenta]#(${lib.getExe pkgs.fastfetch} -l none -c ${./fastfetch-os.jsonc})#[default]"
          set -ga status-left "#[fg=brightblack]@#(${lib.getExe pkgs.fastfetch} -l none -c ${./fastfetch-kernel.jsonc})#[default] "

          set -g status-right-length 100
          set -g status-right " "
          set -ga status-right "#{tmux_mode_indicator} "
          set -ga status-right "#{online_status} "
          set -ga status-right "#[fg=brightcyan]#U#[default]"
          set -ga status-right "#[fg=brightblack]@#[default]"
          set -ga status-right "#{?#{pane_ssh_connected},#[fg=brightgreen],#[fg=brightblack]}#{hostname_short}#[default] "
          set -ga status-right "#[fg=brightblack]{#[default]"
          set -ga status-right "#[fg=brightyellow]${
            lib.replaceStrings
              [ "<" ">" ]
              [
                (lib.concatStrings [
                  "#[fg=brightblack]<#[default]"
                  "#[fg=brightred]"
                ])
                (lib.concatStrings [
                  "#[default]"
                  "#[fg=brightblack]>#[default]"
                  "#[fg=brightyellow]"
                ])
              ]
              env.timeFormat
          }#[default]"
          set -ga status-right "#[fg=brightblack]}#[default]"
        '';
      }
      {
        plugin = online-status.overrideAttrs (attrs: rec {
          version = "master";
          src = pkgs.fetchFromGitHub {
            owner = "tmux-plugins";
            repo = "tmux-${attrs.pluginName}";
            rev = version;
            hash = "sha256-vsR/OfcXK2YL4VmdVku3XxGbR5exgnbmlPVIQ2LnWBg=";
          };
        });
        extraConfig = with config.lib.stylix.colors.withHashtag; ''
          set -g @online_icon "#[fg=${base01},bg=${bright-green}] ONLN #[default]"
          set -g @offline_icon "#[fg=${base01},bg=${bright-red}] OFFLN #[default]"
        '';
      }
      {
        plugin = mkTmuxPlugin rec {
          pluginName = "tmux-current-pane-hostname";
          rtpFilePath = "current_pane_hostname.tmux";
          version = "master";
          src = pkgs.fetchFromGitHub {
            owner = "soyuka";
            repo = pluginName;
            rev = version;
            hash = "sha256-t5jRUvwiwym6f7vfKD/D0Vu1UXpxRMCLNyBbY0nZciw=";
          };
          patches = [ ./tmux-current-pane-hostname.patch ];
        };
      }
      {
        plugin = resurrect;
        extraConfig = ''
          set -g @resurrect-strategy-vim "session"
          set -g @resurrect-strategy-nvim "session"
          set -g @resurrect-capture-pane-contents "on"
          set -g @resurrect-dir "${config.xdg.cacheHome}/tmux/resurrect"
        '';
      }
      {
        plugin = continuum;
        extraConfig = ''
          # set -g @continuum-boot "on"
          set -g @continuum-restore "on"
          set -g @continuum-save-interval "15"
        '';
      }
    ];
    extraConfig = with config.lib.stylix.colors.withHashtag; ''
      set -ga message-command-style "bold"
      set -ga message-style "bold"
      set -g status-justify centre
      set -ga status-style "bold"
      set -ga window-status-bell-style "bold"
      set -g window-status-current-format " #I:#W#F "
      set -g window-status-current-style "fg=${base01},bg=${bright-yellow},bold"
      set -g window-status-format "#I:#W#F"
      set -ga mode-style "bold"
      setw -g pane-border-format ""
      setw -g pane-border-lines single
    '';
    prefix = "C-a";
    tmuxp.enable = true;
  };
}
