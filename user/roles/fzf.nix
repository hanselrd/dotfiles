{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.user.fzf;
in {
  options = {
    roles.user.fzf = {
      enable = lib.mkEnableOption "roles.user.fzf";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.fzf = {
      enable = true;
      tmux.enableShellIntegration = true;
      colors = lib.vendor.nix-colors-custom.fzfThemeFromScheme {scheme = config.colorScheme;};
    };
  };
}
