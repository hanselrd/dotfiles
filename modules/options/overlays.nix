{ lib, ... }: {
  options.overlays = lib.mkOption {
    description = "Overlays";
    type = lib.types.lazyAttrsOf lib.types.raw;
  };
}
