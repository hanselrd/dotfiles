{ config, ... }: {
  programs.delta = {
    enable = true;
    enableGitIntegration = config.programs.git.enable;
    options = {
      syntax-theme = "base16-stylix";
      line-numbers = true;
      navigate = true;
      plus-style = "green bold ul";
      minus-style = "red bold ul";
    };
  };
}
