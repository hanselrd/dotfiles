{
  inputs,
  config,
  lib,
  pkgs,
  env,
  profile,
  ...
}:
let
  cfg = config.roles.system.home-manager;
in
{
  options = {
    roles.system.home-manager = {
      enable = lib.mkEnableOption "roles.system.home-manager";
    };
  };

  config = lib.mkIf cfg.enable {
    home-manager.backupFileExtension = lib.mkForce env.extra.backupFileExtension;
    home-manager.useGlobalPkgs = true;
    home-manager.useUserPackages = lib.mkIf (!lib.profiles.isSystemGaruda) true;
    home-manager.users.${env.user.username} = import ../../user/profiles/${profile.user}.nix;

    home-manager.sharedModules = lib.vendor.home-manager.modules;

    home-manager.extraSpecialArgs = {
      inherit inputs env profile;
    };
  };
}
