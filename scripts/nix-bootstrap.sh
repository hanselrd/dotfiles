#!/usr/bin/env sh
set -xe

. lib/common.sh

NIX_TEMP_DIR=$(make_temp_dir "nix.XXXXXX")

rm -rf "$HOME/.nix-profile"
mkdir -p "$HOME/.config/nix"
rm -rf "$HOME/.config/nix/nix.conf"
printf "experimental-features = nix-command flakes\nsandbox = true\nshow-trace = true\n" > "$HOME/.config/nix/nix.conf"

curl -Lo "$NIX_TEMP_DIR/install.sh" https://nixos.org/nix/install
chmod +x "$NIX_TEMP_DIR/install.sh"
"$NIX_TEMP_DIR/install.sh" --no-daemon

rm -rf "$NIX_TEMP_DIR"
