{ lib, ... }: {
  options.tests = lib.mkOption {
    description = "Tests";
    type = lib.types.lazyAttrsOf lib.types.raw;
  };
}
