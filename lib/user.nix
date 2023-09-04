{
  lib,
  pkgs,
  env,
  ...
}:
(import ../core/user/lib/index.nix) {
  inherit pkgs lib env;
}
