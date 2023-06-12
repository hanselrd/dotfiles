{
  lib,
  pkgs,
  env,
  ...
}: rec {
  mkProgram = name: attrs:
    lib.attrsets.recursiveUpdate
    (
      (import ../program/${name}/default.nix) {
        inherit pkgs lib;
        inherit (pkgs) config;
        inherit env;
      }
    )
    ({enable = true;} // attrs);

  mkProgramIf = name: cond: attrs:
    lib.modules.mkIf cond (mkProgram name attrs);

  mkService = name: attrs:
    lib.attrsets.recursiveUpdate
    (
      (import ../service/${name}/default.nix) {
        inherit pkgs lib;
        inherit (pkgs) config;
        inherit env;
      }
    )
    ({enable = true;} // attrs);

  mkServiceIf = name: cond: attrs:
    lib.modules.mkIf cond (mkService name attrs);
}
