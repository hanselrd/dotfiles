{
  modules.home.nickel-development = { pkgs, ... }: {
    home.packages = with pkgs; [
      nickel
      nls
      topiary
    ];
  };
}
