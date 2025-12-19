{
  config,
  lib,
  pkgs,
  env,
  ...
}:
{
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
    };
  };
}
