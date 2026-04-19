{
  config,
  lib,
  pkgs,
  ...
}:
{
  programs.go = {
    enable = true;
    env = rec {
      GOPATH = "${config.home.homeDirectory}/.go";
      GOBIN = "${GOPATH}/bin";
    };
  };

  home.packages = with pkgs; [
    (lib.lowPrio gotools)
    cobra-cli
    delve
    enumer
    go-task
    gofumpt
    golangci-lint
    golines
    gopls
    ragel
    templ
  ];
}
