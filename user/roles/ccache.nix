{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.roles.user.ccache;
in
{
  options = {
    roles.user.ccache = {
      enable = lib.mkEnableOption "roles.user.ccache";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      ccache
    ];

    xdg.configFile."ccache/ccache.conf" = {
      text = ''
        compression = true
        compression_level = 6
        max_size = 30G
        sloppiness = pch_defines,time_macros
      '';
    };
  };
}
