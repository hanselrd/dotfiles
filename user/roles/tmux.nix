{
  config,
  lib,
  pkgs,
  env,
  ...
}: let
  cfg = config.roles.user.tmux;
in {
  options = {
    roles.user.tmux = {
      enable = lib.mkEnableOption "roles.user.tmux";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.tmux = {
      enable = true;
      clock24 = true;
      customPaneNavigationAndResize = true;
      historyLimit = 50000;
      keyMode = "vi";
      # mouse = true;
      newSession = true;
      terminal = "tmux-256color";
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
          extraConfig = ''
            set -g @mode_indicator_prefix_mode_style "bg=red,fg=#000000"
            set -g @mode_indicator_prefix_prompt "WAIT"
            set -g @mode_indicator_copy_mode_style "bg=yellow,fg=#000000"
            set -g @mode_indicator_copy_prompt "COPY"
            set -g @mode_indicator_sync_mode_style "bg=cyan,fg=#000000"
            set -g @mode_indicator_sync_prompt "SYNC"
            set -g @mode_indicator_empty_mode_style "bg=green,fg=#000000"
            set -g @mode_indicator_empty_prompt "TMUX"

            set -g status-left-length 100
            set -g status-left ""
            set -ga status-left "#{?client_prefix,#[fg=red],#[fg=green]}[#S]#[default] "
            set -ga status-left "#[fg=blue]#(${lib.getExe' pkgs.coreutils "uname"} -r)#[default] "

            set -g status-right-length 100
            set -g status-right ""
            set -ga status-right "#{tmux_mode_indicator} "
            set -ga status-right "#[fg=magenta]#{online_status}#[default] "
            set -ga status-right "#[fg=cyan]#U#[default]"
            set -ga status-right "#[fg=black]@#[default]"
            set -ga status-right "#{?pane_ssh_connected,#[fg=green],#[fg=black]}#{hostname_short}#[default] "
            set -ga status-right "#[fg=yellow]${
              builtins.replaceStrings ["<" ">"] [
                (lib.concatStrings [
                  "#[fg=black]<#[default]"
                  "#[fg=red]"
                ])
                (lib.concatStrings [
                  "#[default]"
                  "#[fg=black]>#[default]"
                  "#[fg=yellow]"
                ])
              ]
              env.extra.timeFormat
            }#[default]"
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
          extraConfig = ''
            set -g @online_icon "ONL"
            set -g @offline_icon "OFFL"
          '';
        }
        {
          plugin =
            mkTmuxPlugin
            rec {
              pluginName = "tmux-current-pane-hostname";
              rtpFilePath = "current_pane_hostname.tmux";
              version = "master";
              src = pkgs.fetchFromGitHub {
                owner = "soyuka";
                repo = pluginName;
                rev = version;
                hash = "sha256-vmGdHAWpYwo95tJNZlu9M5ZaC0qazTP4vT7tUAZHPfA=";
              };
            };
        }
        {
          plugin = resurrect;
          extraConfig = ''
            set -g @resurrect-strategy-vim "session"
            set -g @resurrect-strategy-nvim "session"
            set -g @resurrect-capture-pane-contents "on"
          '';
        }
        {
          plugin = continuum;
          extraConfig = ''
            set -g @continuum-restore "on"
            set -g @continuum-boot "on"
            set -g @continuum-save-interval "15"
          '';
        }
      ];
      extraConfig = ''
        ${builtins.readFile (lib.vendor.nix-colors-custom.tmuxThemeFromScheme {scheme = config.colorScheme;})}

        set -g clock-mode-colour "white"
        set -g message-command-style "fg=yellow,bg=black"
        set -g message-style "fg=yellow,bg=black"
        set -g status-justify centre
        set -g status-style "fg=white,bg=black,bold"
        set -g window-status-activity-style "fg=black,bg=red"
        set -g window-status-bell-style "fg=black,bg=red,bold"
        set -g window-status-current-format "[#I]:#W#F"
        set -g window-status-current-style "fg=#000000,bg=yellow,bold"
        set -g window-status-format "[#I]:#W#F"
        set -g window-status-last-style "fg=yellow,bg=black"
        set -g window-status-style "fg=magenta,bg=black"
        setw -g pane-active-border-style "fg=white,bg=black"
        setw -g pane-border-format ""
        setw -g pane-border-lines single
        setw -g pane-border-style "fg=white,bg=black"
      '';
      prefix = "C-a";
      tmuxp.enable = true;
    };
  };
}
