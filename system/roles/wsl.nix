{
  options,
  config,
  lib,
  pkgs,
  env,
  ...
}: let
  cfg = config.roles.system.wsl;
in {
  options = {
    roles.system.wsl = {
      enable = lib.mkEnableOption "roles.system.wsl";
    };
  };

  config = lib.mkIf cfg.enable (
    {}
    // lib.optionalAttrs (builtins.hasAttr "wsl" options) {
      wsl = {
        enable = true;
        defaultUser = env.user.name;
        # docker-desktop.enable = true;
        # startMenuLaunchers = true;
      };
    }
  );
}
