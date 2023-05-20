{
  config,
  lib,
  pkgs,
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
  };

  # home.sessionVariables = {
  #   LD_PRELOAD = "/usr/lib64/libnss_sss.so.2";
  # };
}
