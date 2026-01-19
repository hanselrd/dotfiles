{ inputs, ... }:
with inputs;
[
  agenix.overlays.default
  rust-overlay.overlays.default
  zig-overlay.overlays.default
  (final: prev: {
    # nix = prev.nixVersions.latest;
    nix = prev.nixVersions.nix_2_32;
    nix-plugins = prev.nix-plugins.overrideAttrs {
      buildInputs = with final; [
        boost
        nix
      ];
    };
  })
  (final: _prev: {
    stable = import nixpkgs-stable {
      inherit (final) system;
      config = {
        allowUnfree = true;
        allowUnfreePredicate = _: true;
      };
    };
  })
]
