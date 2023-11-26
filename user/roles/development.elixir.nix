{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.user.development.elixir;
in {
  options = {
    roles.user.development.elixir = {
      enable = lib.mkEnableOption "roles.user.development.elixir";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      elixir
    ];
  };
}
