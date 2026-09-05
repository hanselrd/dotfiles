{
  modules.home.shell-development = { pkgs, ... }: {
    home.packages = with pkgs; [
      shellcheck
      shfmt
    ];
  };
}
