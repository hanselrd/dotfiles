{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.user.alacritty;
in {
  options = {
    roles.user.alacritty = {
      enable = lib.mkEnableOption "roles.user.alacritty";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.alacritty = {
      enable = true;
      settings = {
        # env = {
        #   WINIT_X11_SCALE_FACTOR = 1;
        # };
        window = {
          padding = {
            x = 15;
            y = 15;
          };
        };
        # font = {
        #   normal = {
        #     family = "";
        #     style = "";
        #   };
        #   size = 10;
        # };
        colors = lib.vendor.nix-colors-custom.alacrittyThemeFromScheme {scheme = config.colorScheme;};
      };
    };
  };
}
