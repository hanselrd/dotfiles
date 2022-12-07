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

  home.packages = with pkgs; [
    nix-index
  ];
}
