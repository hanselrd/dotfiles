{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./common/index.nix
    ./cpp/index.nix
    ./dhall/index.nix
    ./elixir/index.nix
    ./go/index.nix
    ./haskell/index.nix
    ./java/index.nix
    ./lua/index.nix
    ./nickel/index.nix
    ./nix/index.nix
    ./nodejs/index.nix
    ./purescript/index.nix
    ./python/index.nix
    ./rust/index.nix
    ./shell/index.nix
  ];
}
