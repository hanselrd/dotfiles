#!/usr/bin/env sh
set -xe

NIX_INSTALL_DIR=$(mktemp -d "${TMPDIR:-/tmp}/nix.XXXXXX")

# Install Nix: the package manager
curl -Lo "$NIX_INSTALL_DIR/install.sh" https://nixos.org/nix/install
chmod +x "$NIX_INSTALL_DIR/install.sh"
"$NIX_INSTALL_DIR/install.sh" --no-daemon
