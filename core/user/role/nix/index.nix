{
  config,
  lib,
  pkgs,
  ...
}: {
  nixpkgs.config.allowUnfree = true;

  xdg.configFile = {
    "nix/nix.conf".text = ''
      experimental-features = nix-command flakes
      # sandbox = false
    '';
  };

  programs.nix-index = lib.core.user.mkProgram "nix-index" {};
}