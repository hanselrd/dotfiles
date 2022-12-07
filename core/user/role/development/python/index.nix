{
  config,
  lib,
  pkgs,
  ...
}: {
  home.packages = with pkgs; [
    python2
    (python3.withPackages
      (packages:
        with packages; [
          black
          flake8
          pylint
          python-magic
        ]))
    poetry
  ];
}
