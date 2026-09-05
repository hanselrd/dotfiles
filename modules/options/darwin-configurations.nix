{ lib, ... }: {
  options.darwinConfigurations = lib.mkOption {
    description = "Darwin configurations";
    type = lib.types.lazyAttrsOf lib.types.raw;
  };
}
