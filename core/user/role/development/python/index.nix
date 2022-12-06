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
          black
          flake8
          pylint
        ]))
  ];
}
