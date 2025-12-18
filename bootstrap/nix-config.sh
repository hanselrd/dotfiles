#!/usr/bin/env sh
# set -xe

export NIX_CONFIG=$(
  cat << "EOF"
experimental-features = nix-command flakes pipe-operators
show-trace = true
EOF
)
