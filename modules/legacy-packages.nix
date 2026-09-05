{
  self,
  inputs,
  lib,
  ...
}:
let
  inherit (inputs) nixpkgs;
in
{
  legacyPackages = self.lib.eachSystem (
    system:
    import nixpkgs {
      inherit system;
      overlays = lib.attrValues self.overlays;
      config = {
        allowUnfree = true;
        allowUnfreePredicate = _: true;
      };
    }
  );
}
