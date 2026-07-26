{
  config,
  lib,
  pkgs,
  ...
}:
{
  home.packages = with pkgs; [ lldb ];

  home.file.".lldbinit" = {
    text = with config.lib.stylix.colors.withHashtag; ''
      settings set target.x86-disassembly-flavor intel
      settings set auto-confirm true
      settings set prompt ${
        lib.x.pastelText {
          inherit pkgs;
          fgColor = bright-red;
          bold = true;
          escapeStyle = "hex";
        } "(lldb) "
      }
    '';
  };
}
