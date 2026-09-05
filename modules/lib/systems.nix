{ lib, ... }: {
  lib = rec {
    systems = [
      "aarch64-darwin"
      "aarch64-linux"
      # "x86_64-darwin"
      "x86_64-linux"
    ];

    eachSystem = lib.genAttrs systems;
  };
}
