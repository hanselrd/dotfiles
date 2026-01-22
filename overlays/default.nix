{ inputs, ... }:
with inputs;
[
  agenix.overlays.default
  rust-overlay.overlays.default
  zig-overlay.overlays.default
  (final: prev: {
    nix = prev.nixVersions.latest;
    nix-plugins = prev.nix-plugins.overrideAttrs {
      buildInputs = with final; [
        boost
        nix
      ];
      patches = [ ./nix-plugins.patch ];
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
