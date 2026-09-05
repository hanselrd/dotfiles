{
  modules.home.fzf-program = { config, ... }: {
    programs.fzf = {
      enable = true;
      tmux.enableShellIntegration = config.programs.tmux.enable;
    };
  };
}
