{ inputs, ... }:
with inputs;
[
  agenix.overlays.default
  rust-overlay.overlays.default
  zig-overlay.overlays.default
  (final: prev: {
    nix = prev.nixVersions.nix_2_31;
    nix-plugins = prev.nix-plugins.overrideAttrs (attrs: {
      buildInputs = with final; [
        boost
        nix
      ];
      # patches = (attrs.patches or [ ]) ++ [ ./nix-plugins-nix-2.33.patch ];
    });
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
