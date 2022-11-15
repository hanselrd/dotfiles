{
  lib,
  pkgs,
  presets,
  ...
}: {
  mkProgram = name: attrs:
    lib.attrsets.recursiveUpdate
    (
      (import ../program/${name}/default.nix) {
        inherit pkgs lib;
        inherit (pkgs) config;
      }
    )
    ({enable = true;} // attrs);

  mkService = name: attrs:
    lib.attrsets.recursiveUpdate
    (
      (import ../service/${name}/default.nix) {
        inherit pkgs lib;
        inherit (pkgs) config;
      }
    )
    ({
        enable =
          if presets.system == "linux-systemd"
          then true
          else false;
      }
      // attrs);
}
