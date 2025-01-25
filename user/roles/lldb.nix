{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.roles.user.lldb;
in
{
  options = {
    roles.user.lldb = {
      enable = lib.mkEnableOption "roles.user.lldb";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      lldb
    ];

    home.file.".lldbinit" = {
      text = ''
        # TODO: settings
      '';
    };
  };
}
