#!/usr/bin/env sh
set -xe

. lib/common.sh

NIX_TEMP_DIR=$(make_temp_dir "nix.XXXXXX")
NIX_VERSION=2.32.4

rm -rf "$HOME/.nix-profile"
mkdir -p "$HOME/.config/nix"
rm -rf "$HOME/.config/nix/nix.conf"
printf "%s\n" \
  "experimental-features = nix-command flakes pipe-operators" \
  "sandbox = true" \
  "show-trace = true" \
  > "$HOME/.config/nix/nix.conf"

curl -Lo "$NIX_TEMP_DIR/install.sh" "https://releases.nixos.org/nix/nix-$NIX_VERSION/install"
chmod +x "$NIX_TEMP_DIR/install.sh"
"$NIX_TEMP_DIR/install.sh" --no-daemon

rm -rf "$NIX_TEMP_DIR"
