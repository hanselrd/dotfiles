{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.user.development;
in {
  options = {
    roles.user.development = {
      enable = lib.mkEnableOption "roles.user.development";
    };
  };

  config = lib.mkIf cfg.enable {
    roles.user.development.cpp.enable = true;
    roles.user.development.dhall.enable = true;
    roles.user.development.elixir.enable = true;
    roles.user.development.gleam.enable = true;
    roles.user.development.go.enable = true;
    roles.user.development.haskell.enable = true;
    roles.user.development.java.enable = true;
    roles.user.development.kotlin.enable = true;
    roles.user.development.lua.enable = true;
    roles.user.development.nickel.enable = true;
    roles.user.development.nix.enable = true;
    roles.user.development.nodejs.enable = true;
    roles.user.development.purescript.enable = true;
    roles.user.development.python.enable = true;
    roles.user.development.rust.enable = true;
    roles.user.development.shell.enable = true;
    roles.user.development.zig.enable = true;

    home.packages = with pkgs; [
      ctags
      gnumake
      gnupatch
      objconv

      # libGL
      # libGL.dev
      # postgresql
      # postgresql.lib
      # xorg.libX11
      # xorg.libX11.dev
      # xorg.libXcursor.dev
      # xorg.libXext.dev
      # xorg.libXfixes.dev
      # xorg.libXi.dev
      # xorg.libXinerama.dev
      # xorg.libXrandr.dev
      # xorg.libXrender.dev
      # xorg.libxcb.dev
      # (lib.lowPrio xorg.xorgproto)
    ];
  };
}
