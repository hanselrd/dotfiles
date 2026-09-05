{ lib, ... }: {
  options.devShells = lib.mkOption {
    description = "Development shells";
    type = lib.types.lazyAttrsOf (lib.types.lazyAttrsOf lib.types.raw);
  };
}
