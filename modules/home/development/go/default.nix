{ config, pkgs, ... }:
{
  programs.go = {
    enable = true;
    env = rec {
      GOPATH = "${config.home.homeDirectory}/.go";
      GOBIN = "${GOPATH}/bin";
    };
  };

  home.packages = with pkgs; [
    cobra-cli
    delve
    enumer
    go-task
    gofumpt
    golangci-lint
    golines
    gotools
    ragel
    templ
  ];
}
