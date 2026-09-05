{ inputs, ... }:
let
  inherit (inputs) disko;
in
{
  modules.nixos.disko-integration = {
    imports = [ disko.nixosModules.disko ];
  };
}
