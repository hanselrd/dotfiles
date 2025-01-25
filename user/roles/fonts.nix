{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.roles.user.fonts;
in
{
  options = {
    roles.user.fonts = {
      enable = lib.mkEnableOption "roles.user.fonts";
    };
  };

  config = lib.mkIf cfg.enable {
    fonts.fontconfig.enable = true;

    home.packages = with pkgs; [
      liberation_ttf
      nerd-fonts.jetbrains-mono
      noto-fonts
      noto-fonts-cjk-sans
      noto-fonts-emoji
    ];
  };
}
