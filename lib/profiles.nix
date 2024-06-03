{
  lib,
  pkgs,
  profile,
  ...
}: {
  isSystemNixos = profile.system == "nixos";
  isSystemDarwin = profile.system == "darwin";
  isSystemLinux = profile.system == "linux";
  isSystemWsl = profile.system == "wsl";
  isUserBase = profile.user == "base";
  isUserStandard = profile.user == "standard";
  isUserMinimal = profile.user == "minimal";
  isUserFull = profile.user == "full";
}
