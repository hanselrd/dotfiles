{ lib, ... }: {
  options.legacyPackages = lib.mkOption {
    description = "Legacy packages";
    type = lib.types.lazyAttrsOf (lib.types.lazyAttrsOf lib.types.raw);
  };
}
