{ lib, ... }: {
  options.apps = lib.mkOption {
    description = "Applications";
    type = lib.types.lazyAttrsOf (lib.types.lazyAttrsOf lib.types.raw);
  };
}
