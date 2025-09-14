{
  config,
  lib,
  pkgs,
  env,
  ...
}:
let
  cfg = config.roles.user.development.go;
in
{
  options = {
    roles.user.development.go = {
      enable = lib.mkEnableOption "roles.user.development.go";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.go = rec {
      enable = true;
      env = rec {
        GOPATH = "${env.user.homeDirectory}/.go";
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
  };
}
