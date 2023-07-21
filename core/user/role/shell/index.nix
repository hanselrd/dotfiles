{
  config,
  lib,
  pkgs,
  env,
  ...
}: {
  programs.bash = lib.core.user.mkProgram "bash" {};

  programs.zsh = lib.core.user.mkProgram "zsh" {};

  programs.starship = lib.core.user.mkProgram "starship" {};

  home.shellAliases = {
    cd1 = "cd ..";
    cd2 = "cd ../..";
    cd3 = "cd ../../..";
    cd4 = "cd ../../../..";
    cd5 = "cd ../../../../..";
    rcd = "rsync -CcavzP";
    rmv = "rsync -CcavzP --remove-source-files";
    vi = "vim -u NONE -U NONE -N -i NONE";
  };

  home.sessionVariables = lib.modules.mkIf env.shellLdPreload {
    LD_PRELOAD = "/usr/lib64/libnss_sss.so.2";
  };
}
