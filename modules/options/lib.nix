{ lib, ... }: {
  options.lib = lib.mkOption {
    description = "Library";
    type = lib.types.lazyAttrsOf lib.types.raw;
  };
}
