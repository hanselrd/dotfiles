{
  config,
  lib,
  pkgs,
  env,
  ...
}: let
  cfg = config.roles.user.shell;
in {
  options = {
    roles.user.shell = {
      enable = lib.mkEnableOption "roles.user.shell";
    };
  };

  config = lib.mkIf cfg.enable {
    roles.user.bash.enable = true;
    roles.user.zsh.enable = true;
    roles.user.starship.enable = true;

    home.shellAliases = {
      cd1 = "cd ..";
      cd2 = "cd ../..";
      cd3 = "cd ../../..";
      cd4 = "cd ../../../..";
      cd5 = "cd ../../../../..";
      rcp = "rsync -CcavzP";
      rmv = "rsync -CcavzP --remove-source-files";
      sudo = "sudo ";
      vi = "vim -u NONE -U NONE -N -i NONE";
    };

    home.sessionVariables = lib.mkMerge [
      {
        HISTTIMEFORMAT = "%y-%-m-%-d %-H:%M:%S  ";
      }
      (lib.mkIf env.roles.user.shell.ldPreload {
        LD_PRELOAD = "/usr/lib64/libnss_sss.so.2";
      })
    ];
  };
}
