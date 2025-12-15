#!/usr/bin/env sh
set -xe

NIX_ROOT=${1:-$HOME/.nix}

NIX_INSTALL_DIR="$HOME/.bootstrap"
NIX_VERSION=2.32

mkdir -p "$NIX_INSTALL_DIR"
curl -Lo "$NIX_INSTALL_DIR/nix" "https://hydra.nixos.org/job/nix/maintenance-$NIX_VERSION/buildStatic.nix-cli.x86_64-linux/latest/download-by-type/file/binary-dist"
chmod +x "$NIX_INSTALL_DIR/nix"
mkdir -p "$HOME/.config/nix"
rm -rf "$HOME/.config/nix/nix.conf"
printf "%s\n" \
  "experimental-features = nix-command flakes pipe-operators" \
  "store = local?store=$NIX_ROOT/store&state=$NIX_ROOT/var/nix&log=$NIX_ROOT/var/log/nix" \
  "sandbox = true" \
  "show-trace = true" \
  > "$HOME/.config/nix/nix.conf"
"$NIX_INSTALL_DIR/nix" --version
