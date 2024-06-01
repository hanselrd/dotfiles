{
  config,
  lib,
  pkgs,
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
      package = pkgs.tmux.overrideAttrs (finalAttrs: prevAttrs: {
        version = "master";
        src = pkgs.fetchFromGitHub {
          owner = "tmux";
          repo = finalAttrs.pname;
          rev = finalAttrs.version;
          hash = "sha256-99hdAskEByqD4fjl2wrth9QfSkPXkN7o2A9e+BOH6ug=";
        };
        patches = [];
      });
      clock24 = true;
      customPaneNavigationAndResize = true;
      historyLimit = 50000;
      keyMode = "vi";
      # mouse = true;
      newSession = true;
      plugins = with pkgs.tmuxPlugins; [
        # better-mouse-mode
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
            set -g status-left '[#S] #[fg=black,bold]#W#F#[default] #{cpu_fg_color}#{cpu_icon}#{cpu_percentage}#[default] #{ram_fg_color}#{ram_icon}#{ram_percentage}#[default] #{cpu_temp_fg_color}#{cpu_temp_icon}#{cpu_temp}#[default]'
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
            set -g status-right '#{prefix_highlight} #[fg=cyan,bold]#U#[default]#[fg=black,bold]@#[default]#[fg=green,bold]#{?#{pane_ssh_connected},<ssh>,}#[default]#[fg=black,bold]#H#[default] %y-%-m-%-d %-H:%M'
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
              version = "master";
              src = pkgs.fetchFromGitHub {
                owner = "soyuka";
                repo = pluginName;
                rev = version;
                hash = "sha256-vmGdHAWpYwo95tJNZlu9M5ZaC0qazTP4vT7tUAZHPfA=";
              };
            };
        }
      ];
      extraConfig = ''
        ${builtins.readFile (lib.vendor.nix-colors-custom.tmuxThemeFromScheme {scheme = config.colorScheme;})}
      '';
      prefix = "C-a";
      tmuxp.enable = true;
    };
  };
}
