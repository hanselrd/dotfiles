{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.roles.user.bin;
in
{
  options = {
    roles.user.bin = {
      enable = lib.mkEnableOption "roles.user.bin";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      dotfiles-cli
      (lib.common.buildGoBin "df-decrypt")
      (lib.common.buildGoBin "df-encrypt")
      (lib.common.buildGoBin "df-master-key")
    ];
  };
}
