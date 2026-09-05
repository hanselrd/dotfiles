{ inputs, ... }:
let
  inherit (inputs) impermanence;
in
{
  modules.nixos.impermanence-integration = {
    imports = [ impermanence.nixosModules.impermanence ];
  };
}
