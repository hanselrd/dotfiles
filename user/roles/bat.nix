{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.user.bat;
in {
  options = {
    roles.user.bat = {
      enable = lib.mkEnableOption "roles.user.bat";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.bat = {
      enable = true;
      config = {
        theme = "nix-${pkgs.config.colorScheme.slug}";
      };
      themes = {
        "nix-${pkgs.config.colorScheme.slug}" =
          builtins.readFile (lib.vendor.nix-colors-custom.batThemeFromScheme {scheme = pkgs.config.colorScheme;});
      };
    };
  };
}
