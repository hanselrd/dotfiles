#!/usr/bin/env sh
# set -xe

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
