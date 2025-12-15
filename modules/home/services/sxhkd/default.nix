{
  config,
  lib,
  pkgs,
  ...
}:
{
  services.sxhkd = {
    enable = true;
    keybindings = {
      "super + Escape" = "${lib.getExe pkgs.killall} -q -s SIGUSR1 sxhkd";
      "super + Return" = "$TERMINAL";
      "super + shift + Return" = "$TERMINAL -e ${lib.getExe pkgs.dash} -c ranger";
      "super + shift + e" = "$TERMINAL -e ${lib.getExe pkgs.dash} -c $EDITOR";
      "super + shift + z" = "$BROWSER";
      "super + {d,shift + w,shift + s}" = "rofi -show {run,window,ssh}";
      "@Print" = "flameshot gui";
      # TODO: brightness/audio
    };
  };
}
