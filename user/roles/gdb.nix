{
  config,
  lib,
  pkgs,
  env,
  ...
}:
let
  cfg = config.roles.user.gdb;
in
{
  options = {
    roles.user.gdb = {
      enable = lib.mkEnableOption "roles.user.gdb";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      gdb
    ];

    xdg.configFile."gdb/gdbinit" = {
      text = ''
        set disassembly-flavor intel
        set history save on
        set history size 10000
        set history remove-duplicates 100
        set history filename ${env.user.homeDirectory}/.gdb_history
        set print pretty on
        set pagination off
        set confirm off
        set prompt \033[1;31m(gdb) \033[0m
      '';
    };
  };
}
