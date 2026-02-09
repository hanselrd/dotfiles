{ pkgs, ... }:
{
  home.packages = with pkgs; [
    cabal-install
    fourmolu
    ghc
    haskell-language-server
    hlint
    ormolu
    stack
  ];
}
