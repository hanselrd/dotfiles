#!/usr/bin/env sh
# set -xe

source_nix() {
  if [ -f "$HOME/.nix-profile/etc/profile.d/nix.sh" ]; then
    . "$HOME/.nix-profile/etc/profile.d/nix.sh"
  fi
}

make_temp_dir() {
  template=$1
  tmpdir=${2:-${TMPDIR:-/tmp}}

  mktemp -d "$tmpdir/$template"
}

make_temp_file() {
  template=$1
  tmpdir=${2:-${TMPDIR:-/tmp}}

  mktemp "$tmpdir/$template"
}
