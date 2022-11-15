{
  config,
  lib,
  pkgs,
  ...
}: {
  settings = {
    armor = true;
  };
  scdaemonSettings = {
    disable-ccid = true;
  };
}
