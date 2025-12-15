{ lib, ... }@args:
{
  x = lib.mergeAttrsList [
    builtins.extraBuiltins or  {}
    (import ./build.nix args)
    (import ./common.nix args)
  ];
}
