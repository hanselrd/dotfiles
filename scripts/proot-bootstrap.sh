#!/usr/bin/env sh
set -xe

PROOT_INSTALL_DIR="$HOME/.bootstrap"

mkdir -p "$PROOT_INSTALL_DIR"
curl -Lo "$PROOT_INSTALL_DIR/proot" https://proot.gitlab.io/proot/bin/proot
chmod +x "$PROOT_INSTALL_DIR/proot"
