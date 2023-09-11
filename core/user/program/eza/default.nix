{
  config,
  lib,
  pkgs,
  ...
}: {
  enableAliases = true;
  extraOptions = [
    "--group"
    "--group-directories-first"
    "--octal-permissions"
    "--time-style=iso"
  ];
}
