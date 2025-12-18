#!/usr/bin/env sh
set -xe

. lib/common.sh

# NIX_VERSION=${1:-2.33.0}

NIX_TEMP_DIR=$(make_temp_dir "nix.XXXXXX")

# curl -Lo "$NIX_TEMP_DIR/install.sh" "https://releases.nixos.org/nix/nix-$NIX_VERSION/install"
curl -Lo "$NIX_TEMP_DIR/install.sh" "https://nixos.org/nix/install"
chmod +x "$NIX_TEMP_DIR/install.sh"
"$NIX_TEMP_DIR/install.sh" --no-daemon

rm -rf "$NIX_TEMP_DIR"
