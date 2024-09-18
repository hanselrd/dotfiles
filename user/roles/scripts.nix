{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.user.scripts;
in {
  options = {
    roles.user.scripts = {
      enable = lib.mkEnableOption "roles.user.scripts";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      dotfiles-cli
      (lib.common.buildGoScript "df-decrypt")
      (lib.common.buildGoScript "df-encrypt")
      (lib.common.buildGoScript "df-master-key")
    ];
  };
}
