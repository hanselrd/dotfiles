{ pkgs, ... }: {
  home.packages = with pkgs; [
    (haskellPackages.ghcWithPackages (hpkgs: with hpkgs; [ zlib ]))
    cabal-install
    fourmolu
    haskell-language-server
    hlint
    ormolu
    stack
  ];
}
