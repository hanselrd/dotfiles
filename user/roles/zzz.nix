{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.user.zzz;
in {
  options = {
    roles.user.zzz = {
      enable = lib.mkEnableOption "roles.user.zzz";
    };
  };

  config = lib.mkIf cfg.enable {
    roles.user.bat.enable = true;
    roles.user.eza.enable = true;
    roles.user.fzf.enable = true;
    roles.user.git.enable = true;
    roles.user.htop.enable = true;
    roles.user.ssh.enable = true;
    roles.user.tmux.enable = true;
  };
}
