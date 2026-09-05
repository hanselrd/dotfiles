{ self, inputs, ... }:
let
  inherit (inputs) treefmt-nix;

  treefmtEval = self.lib.eachSystem (
    system: treefmt-nix.lib.evalModule self.legacyPackages.${system} (self.outPath + "/treefmt.nix")
  );
in
{
  formatter = self.lib.eachSystem (system: treefmtEval.${system}.config.build.wrapper);

  checks = self.lib.eachSystem (system: {
    formatting = treefmtEval.${system}.config.build.check self;
  });
}
