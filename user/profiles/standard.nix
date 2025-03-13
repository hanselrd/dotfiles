{
  config,
  lib,
  pkgs,
  ...
}:
{
  imports = [
    ./minimal.nix
  ];

  # roles.user.jq.enable = true;
  # roles.user.pandoc.enable = true;
  roles.user.ranger.enable = true;
}
