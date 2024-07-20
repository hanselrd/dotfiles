{
  config,
  lib,
  pkgs,
  env,
  ...
}: let
  cfg = config.roles.system.shell;
in {
  options = {
    roles.system.shell = {
      enable = lib.mkEnableOption "roles.system.shell";
    };
  };

  config = lib.mkIf cfg.enable (
    lib.recursiveUpdate
    {
      programs.zsh.enable = true;
      environment.shells = with pkgs; [zsh];

      users.users.${env.user.username}.shell = pkgs.zsh;
    }
    (lib.optionalAttrs (!lib.profiles.isSystemDarwin) {
      users.defaultUserShell = pkgs.zsh;
    })
  );
}
