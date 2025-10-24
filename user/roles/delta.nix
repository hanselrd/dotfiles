{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.roles.user.delta;
in
{
  options = {
    roles.user.delta = {
      enable = lib.mkEnableOption "roles.user.delta";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.delta = {
      enable = true;
      enableGitIntegration = true;
      options = {
        syntax-theme = "nix-${config.colorScheme.slug}";
        line-numbers = true;
        plus-style = "green bold ul";
        minus-style = "red bold ul";
      };
    };
  };
}
