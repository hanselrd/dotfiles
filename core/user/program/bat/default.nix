{
  config,
  lib,
  pkgs,
  ...
}: {
  config = {
    theme = "nix-${config.colorScheme.slug}";
  };
  themes = {
    "nix-${config.colorScheme.slug}" =
      builtins.readFile (lib.vendor.nix-colors-custom.batThemeFromScheme {scheme = config.colorScheme;});
  };
}
