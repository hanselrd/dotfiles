{ inputs, ... }:
let
  inherit (inputs)
    agenix
    amber
    rust-overlay
    zig-overlay
    ;
in
{
  overlays = {
    agenix = agenix.overlays.default;
    amber = final: _prev: { amber-lang = amber.packages.${final.stdenv.hostPlatform.system}.default; };
    rust-overlay = rust-overlay.overlays.default;
    zig-overlay = zig-overlay.overlays.default;
  };
}
