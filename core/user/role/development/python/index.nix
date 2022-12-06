{
  config,
  lib,
  pkgs,
  ...
}: {
  home.packages = with pkgs; [
    (python3.withPackages
      (packages:
        with packages; [
          # poetry
          black
          flake8
          pylint
        ]))
  ];
}
