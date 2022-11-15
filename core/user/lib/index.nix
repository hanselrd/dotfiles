{
  lib,
  pkgs,
  preset,
  ...
}: {
  mkProgram = name: attrs:
    lib.attrsets.recursiveUpdate
    (
      (import ../program/${name}/default.nix) {
        inherit pkgs lib preset;
        inherit (pkgs) config;
      }
    )
    ({enable = true;} // attrs);

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
