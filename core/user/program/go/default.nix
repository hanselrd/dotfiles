{
  config,
  lib,
  pkgs,
  ...
}: rec {
  goPath = ".go";
  goBin = "${goPath}/bin";
}
