{ ... }: {
  overlays.nix-with-plugins = final: prev: {
    nix = prev.nixVersions.nix_2_34;
    nix-plugins = prev.nix-plugins.overrideAttrs (attrs: {
      buildInputs = with final; [
        boost
        nix
      ];
      patches = (attrs.patches or [ ]) ++ [ ./nix-plugins-nix-2.34.patch ];
    });
  };
}
