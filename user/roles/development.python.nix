{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.roles.user.development.python;
in
{
  options = {
    roles.user.development.python = {
      enable = lib.mkEnableOption "roles.user.development.python";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      (python3.withPackages (
        packages: with packages; [
          black
          flake8
          pylint
          python-magic
        ]
      ))
      pipenv
      poetry
    ];
  };
}
