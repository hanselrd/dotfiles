{
  lib,
  pkgs,
  profile,
  ...
}: {
  isSystemNixos = profile.system == "nixos";
  isSystemDarwin = profile.system == "darwin";
  isSystemWsl = profile.system == "wsl";
  isSystemLinux = profile.system == "linux";
  isUserBase = profile.user == "base";
  isUserStandard = profile.user == "standard";
  isUserMinimal = profile.user == "minimal";
  isUserFull = profile.user == "full";
}
