#!/usr/bin/env sh
# set -xe

tmpdir=${XDG_RUNTIME_DIR:-${TMPDIR:-/tmp}}/nix-$(id -u)

make_temp_dir() {
  template=$1

  mkdir -p "$tmpdir"
  mktemp -d "$tmpdir/$template"
}

make_temp_file() {
  template=$1

  mkdir -p "$tmpdir"
  mktemp "$tmpdir/$template"
}
