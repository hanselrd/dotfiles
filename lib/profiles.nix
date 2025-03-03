{
  lib,
  pkgs,
  profile,
  ...
}:
{
  isSystemNixos = profile.system == "nixos";
  isSystemGaruda = profile.system == "garuda";
  isSystemWsl = profile.system == "wsl";
  isSystemDarwin = profile.system == "darwin";
  isSystemGeneric = profile.system == "generic";
  isUserBase = profile.user == "base";
  isUserStandard = profile.user == "standard";
  isUserMinimal = profile.user == "minimal";
  isUserFull = profile.user == "full";
}
