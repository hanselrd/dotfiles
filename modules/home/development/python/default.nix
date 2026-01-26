{ pkgs, ... }:
{
  home.packages = with pkgs; [
    (python3.withPackages (
      ppkgs: with ppkgs; [
        black
        flake8
        pylint
        python-magic
      ]
    ))
    pipenv
    poetry
    pyright
  ];
}
