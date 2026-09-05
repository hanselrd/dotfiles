{ lib, ... }: {
  options.formatter = lib.mkOption {
    description = "Formatter";
    type = lib.types.lazyAttrsOf lib.types.raw;
  };
}
