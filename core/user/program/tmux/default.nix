{
  config,
  lib,
  pkgs,
  ...
}: {
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
      plugin = resurrect;
      extraConfig = "set -g @resurrect-strategy-nvim 'session'";
    }
    {
      plugin =
        mkTmuxPlugin
        {
          pluginName = "base16-tmux";
          rtpFilePath = "tmuxcolors.tmux";
          version = "unstable";
          src = pkgs.fetchFromGitHub {
            owner = "tinted-theming";
            repo = "base16-tmux";
            rev = "c3366618c6d746647051b3cd0ded23f3f7c332e1";
            sha256 = "lbe/Ov+ShmlA4O/qAYX8K9ZSKvCzekyXKMcSpzfyXvA=";
          };
        };
      extraConfig = "set -g @colors-base16 'chalk'";
    }
  ];
  prefix = "C-a";
  tmuxp.enable = true;
}
