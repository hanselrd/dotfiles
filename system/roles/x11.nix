{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.system.x11;
in {
  options = {
    roles.system.x11 = {
      enable = lib.mkEnableOption "roles.system.x11";
    };
  };

  config = lib.mkIf cfg.enable (
    lib.optionalAttrs (!lib.profiles.isSystemDarwin) {
      services.xserver = {
        # enable = true;
        xkb = {
          layout = "us";
          variant = "";
        };
      };

      # services.displayManager.sddm.enable = true;
      # services.desktopManager.plasma6.enable = true;

      # services.xrdp = {
      #   enable = true;
      #   defaultWindowManager = "startplasma-x11";
      #   openFirewall = true;
      # };
    }
  );
}
