{
  modules.home.zig-development = { pkgs, ... }: {
    home.packages = with pkgs; [
      zigpkgs.master
      zls
    ];
  };
}
