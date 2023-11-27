{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./base.nix
  ];

  roles.user.development.enable = true;
  roles.user.docker.enable = true;

  home.packages = with pkgs; [
    hyperfine
    lm_sensors
  ];
}
