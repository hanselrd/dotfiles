{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.user.sxhkd;
in {
  options = {
    roles.user.sxhkd = {
      enable = lib.mkEnableOption "roles.user.sxhkd";
    };
  };

  config = lib.mkIf cfg.enable {
    services.sxhkd = {
      enable = true;
      keybindings = {
        "super + Escape" = ''
          ${lib.getExe pkgs.killall} -q -s SIGUSR1 sxhkd"
        '';
        "super + Return" = lib.mkIf config.roles.user.terminal.enable ''
          $TERMINAL
        '';
        "super + shift + Return" = lib.mkIf config.roles.user.ranger.enable ''
          $TERMINAL -e ${lib.getExe' pkgs.bash "sh"} -c ranger
        '';
        "super + shift + e" = lib.mkIf config.roles.user.editor.enable ''
          $TERMINAL -e ${lib.getExe' pkgs.bash "sh"} -c $EDITOR
        '';
        "super + shift + z" = lib.mkIf config.roles.user.browser.enable ''
          $BROWSER
        '';
        "super + {d,shift + w,shift + s}" = lib.mkIf config.roles.user.rofi.enable ''
          rofi -show {run,window,ssh}
        '';
        "@Print" = lib.mkIf config.roles.users.flameshot.enable ''
          flameshot gui
        '';
        # TODO: brightness/audio
      };
    };
  };
}
