{
  self,
  inputs,
  lib,
  ...
}:
let
  inherit (inputs) stylix;

  theme = "catppuccin-mocha";
in
{
  modules = self.lib.eachModule (module: {
    stylix-integration = { pkgs, ... }: {
      imports = lib.flatten [
        (lib.optional (module == "nixos") stylix.nixosModules.stylix)
        (lib.optional (module == "darwin") stylix.darwinModules.stylix)
        (lib.optional (module == "home") stylix.homeModules.stylix)
      ];

      stylix = lib.mergeAttrsList [
        {
          enable = true;
          base16Scheme = "${pkgs.base16-schemes}/share/themes/${theme}.yaml";
        }
        (lib.optionalAttrs
          (lib.elem module [
            "nixos"
            "darwin"
          ])
          {
            homeManagerIntegration = {
              autoImport = false;
              followSystem = false;
            };
          }
        )
      ];
    };
  });
}
