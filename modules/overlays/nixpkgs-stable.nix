{
  self,
  inputs,
  lib,
  ...
}:
let
  inherit (inputs) nixpkgs-stable;
in
{
  overlays.nixpkgs-stable = final: _prev: {
    stable = import nixpkgs-stable {
      inherit (final) system;
      overlays = lib.attrValues self.overlays;
      config = {
        allowUnfree = true;
        allowUnfreePredicate = _: true;
      };
    };
  };
}
