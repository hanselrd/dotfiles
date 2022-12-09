#!/usr/bin/env sh
set -xe

NIX_TEMP_DIR=$(mktemp -d "${TMPDIR:-/tmp}/nix.XXXXXX")

curl -Lo "$NIX_TEMP_DIR/install.sh" https://nixos.org/nix/install
chmod +x "$NIX_TEMP_DIR/install.sh"
"$NIX_TEMP_DIR/install.sh" --no-daemon

rm -rf "$NIX_TEMP_DIR"
