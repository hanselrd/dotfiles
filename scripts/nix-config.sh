#!/bin/sh
_nixConfig='experimental-features = nix-command flakes pipe-operators
show-trace = true'
export 'NIX_CONFIG='"$_nixConfig"
