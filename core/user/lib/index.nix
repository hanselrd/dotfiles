{
  lib,
  pkgs,
  preset,
  ...
}: rec {
  mkProgram = name: attrs:
    lib.attrsets.recursiveUpdate
    (
      (import ../program/${name}/default.nix) {
        inherit pkgs lib preset;
        inherit (pkgs) config;
      }
    )
    ({enable = true;} // attrs);

  mkProgramIf = name: cond: attrs:
    if cond
    then mkProgram name attrs
    else {};

  mkService = name: attrs:
    lib.attrsets.recursiveUpdate
    (
      (import ../service/${name}/default.nix) {
        inherit pkgs lib preset;
        inherit (pkgs) config;
      }
    )
    ({
        enable =
          if preset.system == "linux-systemd"
          then true
          else false;
      }
      // attrs);
}
