{
  config,
  lib,
  pkgs,
  ...
}:
{
  home.packages = with pkgs; [ gdb ];

  xdg.configFile."gdb/gdbinit" = {
    text = with config.lib.stylix.colors.withHashtag; ''
      set disassembly-flavor intel
      set history save on
      set history size 10000
      set history remove-duplicates 100
      set history filename ${config.home.homeDirectory}/.gdb_history
      set print pretty on
      set pagination off
      set confirm off
      set prompt ${
        lib.replaceStrings [ "\\033" "m" ] [ "\\001\\033" "m\\002" ] (
          lib.x.pastelText {
            inherit pkgs;
            fgColor = bright-red;
            bold = true;
            escapeStyle = "octal";
          } "(gdb) "
        )
      }
    '';
  };
}
