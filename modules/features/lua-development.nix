{
  modules.home.lua-development = { pkgs, ... }: {
    home.packages = with pkgs; [
      lua
      lua-language-server
      luarocks
      stylua
    ];
  };
}
