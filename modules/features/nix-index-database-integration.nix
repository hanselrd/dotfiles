{
  self,
  inputs,
  lib,
  ...
}:
let
  inherit (inputs) nix-index-database;
in
{
  modules = self.lib.eachModule (module: {
    nix-index-database-integration = {
      imports = lib.flatten [
        (lib.optional (module == "nixos") nix-index-database.nixosModules.default)
        (lib.optional (module == "darwin") nix-index-database.darwinModules.nix-index)
        (lib.optional (module == "home") nix-index-database.homeModules.default)
      ];

      programs.nix-index-database.comma.enable = true;
    };
  });
}
