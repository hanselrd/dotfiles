{
  config,
  lib,
  pkgs,
  env,
  ...
}:
let
  cfg = config.roles.user.user;
in
{
  options = {
    roles.user.user = {
      enable = lib.mkEnableOption "roles.user.user";
    };
  };

  config = lib.mkIf cfg.enable {
    home.sessionVariables = lib.mkMerge [
      (lib.mkIf env.roles.user.user.overwrite { USER = env.user.username; })
    ];
  };
}
