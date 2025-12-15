{
  config,
  lib,
  pkgs,
  env,
  ...
}:
{
  home.packages = with pkgs; [ gdb ];

  xdg.configFile."gdb/gdbinit" = {
    text = ''
      set disassembly-flavor intel
      set history save on
      set history size 10000
      set history remove-duplicates 100
      set history filename ${config.home.homeDirectory}/.gdb_history
      set print pretty on
      set pagination off
      set confirm off
      set prompt ${
        lib.replaceStrings [ "\\e" ] [ "\\033" ] (
          lib.x.ansiText {
            inherit pkgs;
            style = "yellow bold";
            escapeStyle = "bash";
          } "(gdb) "
        )
      }
    '';
  };
}
