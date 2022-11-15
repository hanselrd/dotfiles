{
  config,
  lib,
  pkgs,
  ...
}: {
  enableExtraSocket = true;
  enableSshSupport = true;
  pinentryFlavor = "curses";
}
