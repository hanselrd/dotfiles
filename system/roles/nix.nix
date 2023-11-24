{
  self,
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.system.nix;
in {
  options = {
    roles.system.nix = {
      enable = lib.mkEnableOption "roles.system.nix";
    };
  };

  config = lib.mkIf cfg.enable {
    system.configurationRevision = self.rev or "dirty";

    nix.settings.trusted-users = ["root" "@wheel"];

    nixpkgs.config.allowUnfree = true;

    system.stateVersion = "22.05"; # Did you read the comment?
  };
}
