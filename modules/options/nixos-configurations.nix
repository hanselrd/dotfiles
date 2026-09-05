{ lib, ... }: {
  options.nixosConfigurations = lib.mkOption {
    description = "NixOS configurations";
    type = lib.types.lazyAttrsOf lib.types.raw;
  };
}
