{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.system.shell;
in {
  options = {
    roles.system.shell = {
      enable = lib.mkEnableOption "roles.system.shell";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.zsh.enable = true;
    environment.shells = with pkgs; [zsh];

    users.defaultUserShell = pkgs.zsh;
  };
}
