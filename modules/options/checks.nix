{ lib, ... }: {
  options.checks = lib.mkOption {
    description = "Checks";
    type = lib.types.lazyAttrsOf (lib.types.lazyAttrsOf lib.types.raw);
  };
}
