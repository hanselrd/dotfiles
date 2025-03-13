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
  isUserMinimal = profile.user == "minimal";
  isUserStandard = profile.user == "standard";
  isUserFull = profile.user == "full";
}
