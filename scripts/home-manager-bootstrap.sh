#!/usr/bin/env sh
set -xe

. lib/common.sh

source_nix

HMB_CONFIGURATION="${1:-linux-base}"

nix build --no-link ".#homeConfigurations.$HMB_CONFIGURATION.activationPackage" --impure --extra-experimental-features "nix-command flakes" --accept-flake-config
HMB_HOME_MANAGER="$(nix path-info ".#homeConfigurations.$HMB_CONFIGURATION.activationPackage" --impure --extra-experimental-features "nix-command flakes" --accept-flake-config)"/home-path/bin/home-manager
$HMB_HOME_MANAGER switch --flake ".#$HMB_CONFIGURATION" -b bak."$(date +"%Y%m%d")" --impure --extra-experimental-features "nix-command flakes" --option accept-flake-config true
