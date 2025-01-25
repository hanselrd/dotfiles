{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.roles.user.development.lua;
in
{
  options = {
    roles.user.development.lua = {
      enable = lib.mkEnableOption "roles.user.development.lua";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      lua
      luarocks
      stylua
    ];
  };
}
