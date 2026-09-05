{ lib, ... }: {
  options.homeConfigurations = lib.mkOption {
    description = "Home configurations";
    type = lib.types.lazyAttrsOf lib.types.raw;
  };
}
