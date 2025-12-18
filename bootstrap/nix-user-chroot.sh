#!/usr/bin/env sh
set -xe

NIX_USER_CHROOT_VERSION=${1:-1.2.2}
NIX_USER_CHROOT_ARCH=${2:-x86_64}

NIX_USER_CHROOT_INSTALL_DIR="$HOME/.bootstrap"

mkdir -p "$NIX_USER_CHROOT_INSTALL_DIR"
curl -Lo "$NIX_USER_CHROOT_INSTALL_DIR/nix-user-chroot" "https://github.com/nix-community/nix-user-chroot/releases/download/$NIX_USER_CHROOT_VERSION/nix-user-chroot-bin-$NIX_USER_CHROOT_VERSION-$NIX_USER_CHROOT_ARCH-unknown-linux-musl"
chmod +x "$NIX_USER_CHROOT_INSTALL_DIR/nix-user-chroot"
