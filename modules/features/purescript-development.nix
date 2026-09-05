{
  modules.home.purescript-development = { pkgs, ... }: {
    home.packages = with pkgs; [
      purescript
      spago
    ];
  };
}
