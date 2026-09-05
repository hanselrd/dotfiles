{
  self,
  inputs,
  lib,
  ...
}:
let
  inherit (inputs) home-manager;
in
{
  modules = self.lib.eachModule (module: {
    home-manager-integration = {
      imports = lib.flatten [
        (lib.optional (module == "nixos") home-manager.nixosModules.home-manager)
        (lib.optional (module == "darwin") home-manager.darwinModules.home-manager)
      ];

      config = lib.mergeAttrsList [
        (lib.optionalAttrs
          (lib.elem module [
            "nixos"
            "darwin"
          ])
          {
            home-manager = {
              backupFileExtension = "bkp.${self.lib.builtins.getRandomString 5}";
              useGlobalPkgs = true;
              useUserPackages = true;
            };
          }
        )
      ];
    };
  });
}
