{
  config,
  lib,
  pkgs,
  ...
}: {
  home.packages = with pkgs; [
    ctags
    gnumake
    gnupatch

    libGL
    libGL.dev
    postgresql
    postgresql.lib
    xorg.libX11
    xorg.libX11.dev
    xorg.libXcursor.dev
    xorg.libXext.dev
    xorg.libXfixes.dev
    xorg.libXi.dev
    xorg.libXinerama.dev
    xorg.libXrandr.dev
    xorg.libXrender.dev
    xorg.libxcb.dev
    (lib.meta.lowPrio xorg.xorgproto)
  ];
}
