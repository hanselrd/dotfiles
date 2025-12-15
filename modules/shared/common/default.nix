{
  inputs,
  overlays,
  config,
  lib,
  pkgs,
  rootPath,
  env,
  ...
}:
{
  nixpkgs = {
    inherit overlays;
    config = {
      allowUnfree = true;
      allowUnfreePredicate = _: true;
    };
  };

  nix.registry = lib.mapAttrs (_: flake: { inherit flake; }) (
    lib.filterAttrs (_: lib.isType "flake") inputs
  );

  nix.settings = {
    experimental-features = [
      "nix-command"
      "flakes"
      "pipe-operators"
    ];
    store = lib.mkIf (
      env.nixRoot != null
    ) "local?store=${env.nixRoot}/store&state=${env.nixRoot}/var/nix&log=${env.nixRoot}/var/log/nix";
    sandbox = true;
    show-trace = true;
    plugin-files = "${pkgs.nix-plugins}/lib/nix/plugins";
    extra-builtins-file = rootPath + "/lib/builtins.nix";
  };

  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 7d";
  };

  nix.package = lib.mkForce pkgs.nix;

  stylix = {
    enable = true;
    base16Scheme = "${pkgs.base16-schemes}/share/themes/${env.theme}.yaml";
  };
}
