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

  config = lib.mkIf cfg.enable (
    lib.recursiveUpdate
    {
      users.users.${env.user.username} = {
        description = env.user.name;
      };
    }
    (lib.optionalAttrs (!lib.profiles.isSystemDarwin) {
      users.users.${env.user.username} = {
        isNormalUser = true;
        extraGroups = lib.flatten [
          "wheel"
          (lib.optional config.roles.system.docker.enable "docker")
          (lib.optional config.roles.system.networking.enable "networkmanager")
        ];
        initialPassword = "password";
      };
    })
  );
}
