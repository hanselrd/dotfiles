{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.roles.user.development.shell;
in
{
  options = {
    roles.user.development.shell = {
      enable = lib.mkEnableOption "roles.user.development.shell";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      shellcheck
      shfmt
    ];
  };
}
