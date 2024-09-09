{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.user.fonts;
in {
  options = {
    roles.user.fonts = {
      enable = lib.mkEnableOption "roles.user.fonts";
    };
  };

  config = lib.mkIf cfg.enable {
    fonts.fontconfig.enable = true;

    home.packages = with pkgs; [
      (nerdfonts.override {fonts = ["JetBrainsMono"];})
      liberation_ttf
      noto-fonts
      noto-fonts-cjk
      noto-fonts-emoji
      noto-fonts-extra
    ];
  };
}
