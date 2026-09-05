{
  modules.home.elixir-development = { pkgs, ... }: {
    home.packages = with pkgs; [
      elixir
      elixir-ls
    ];
  };
}
