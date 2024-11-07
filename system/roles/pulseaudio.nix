{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.system.pulseaudio;
in {
  options = {
    roles.system.pulseaudio = {
      enable = lib.mkEnableOption "roles.system.pulseaudio";
    };
  };

  config = lib.mkIf cfg.enable (
    lib.optionalAttrs (!lib.profiles.isSystemDarwin) {
      hardware.pulseaudio.enable = true;

      # nixpkgs.config.pulseaudio = true;
    }
  );
}
