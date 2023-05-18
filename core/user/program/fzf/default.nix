{
  config,
  lib,
  pkgs,
  ...
}: {
  tmux.enableShellIntegration = true;
  colors = lib.vendor.nix-colors-custom.fzfThemeFromScheme {scheme = config.colorScheme;};
}
