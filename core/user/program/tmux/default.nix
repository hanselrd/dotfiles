{
  config,
  lib,
  pkgs,
  ...
}: {
  keyMode = "vi";
  plugins = with pkgs.tmuxPlugins; [
    cpu
    {
      plugin = resurrect;
      extraConfig = "set -g @resurrect-strategy-nvim 'session'";
    }
    {
      plugin = continuum;
      extraConfig = ''
        set - g @continuum-restore 'on'
              set -g @continuum-save-interval '60' # minutes
      '';
    }
  ];
  prefix = "C-a";
  tmuxp.enable = true;
}
