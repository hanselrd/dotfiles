{ lib, ... }@args:
{
  x = lib.mergeAttrsList [
    builtins.extraBuiltins
    (import ./build.nix args)
    (import ./common.nix args)
  ];
}
