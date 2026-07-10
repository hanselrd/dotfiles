#!/bin/sh
set '-euxo' pipefail
_dir="$1"
shift
sudo mkdir '-pm' 0755 '/nix' "$_dir"
sudo chown "$USER" '/nix' "$_dir"
sudo mount '--bind' "$_dir" '/nix'
