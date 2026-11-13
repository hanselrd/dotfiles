{ lib, ... }: {
  options.packages = lib.mkOption {
    description = "Packages";
    type = lib.types.lazyAttrsOf (lib.types.lazyAttrsOf lib.types.raw);
  };
}
