{
  config,
  lib,
  pkgs,
  env,
  ...
}:
let
  cfg = config.roles.user.shell;
in
{
  options = {
    roles.user.shell = {
      enable = lib.mkEnableOption "roles.user.shell";
    };
  };

  config = lib.mkIf cfg.enable {
    # roles.user.starship.enable = true;
    roles.user.bash.enable = true;
    roles.user.oh-my-posh.enable = true;
    roles.user.zsh.enable = true;

    home.shellAliases = {
      cd1 = "cd ..";
      cd2 = "cd ../..";
      cd3 = "cd ../../..";
      cd4 = "cd ../../../..";
      cd5 = "cd ../../../../..";
      rcp = "rsync -CcavzP";
      rmv = "rsync -CcavzP --remove-source-files";
      shroot = "sudo -E $SHELL";
      sudo = "sudo ";
      vi = "vim -u NONE -U NONE -N -i NONE";
    };

    home.sessionVariables = {
      HISTTIMEFORMAT = "${env.extra.timeFormat}  ";
      LD_LIBRARY_PATH = "$LD_LIBRARY_PATH\${LD_LIBRARY_PATH:+:}${pkgs.sssd}/lib";
    };
  };
}
