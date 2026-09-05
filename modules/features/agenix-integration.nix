{
  self,
  inputs,
  lib,
  ...
}:
let
  inherit (inputs) agenix;
in
{
  modules = self.lib.eachModule (module: {
    agenix-integration = {
      imports = lib.flatten [
        (lib.optional (module == "nixos") agenix.nixosModules.default)
        (lib.optional (module == "darwin") agenix.darwinModules.default)
        (lib.optional (module == "home") agenix.homeManagerModules.default)
      ];
    };
  });
}
