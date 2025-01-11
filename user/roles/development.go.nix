{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.user.development.go;
in {
  options = {
    roles.user.development.go = {
      enable = lib.mkEnableOption "roles.user.development.go";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.go = rec {
      enable = true;
      goPath = ".go";
      goBin = "${goPath}/bin";
    };

    home.packages = with pkgs; [
      cobra-cli
      delve
      enumer
      go-task
      gofumpt
      golines
      gotools
      templ
    ];
  };
}
