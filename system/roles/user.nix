{
  config,
  lib,
  pkgs,
  env,
  ...
}: let
  cfg = config.roles.system.user;
in {
  options = {
    roles.system.user = {
      enable = lib.mkEnableOption "roles.system.user";
    };
  };

  config = lib.mkIf cfg.enable {
    users.users.${pkgs.config.home.username} = {
      isNormalUser = true;
      description = env.user.name;
      extraGroups = ["networkmanager" "wheel" "docker"];
      initialPassword = "password";
    };
  };
}
