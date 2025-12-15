{
  config,
  lib,
  pkgs,
  ...
}:
{
  home.packages = with pkgs; [ ccache ];

  xdg.configFile."ccache/ccache.conf" = {
    text = ''
      compression = true
      compression_level = 6
      max_size = 30G
      sloppiness = pch_defines,time_macros
    '';
  };
}
