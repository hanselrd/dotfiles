#!/usr/bin/env sh
set -xe

if [ -f "$HOME/.nix-profile/etc/profile.d/nix.sh" ]; then
    . "$HOME/.nix-profile/etc/profile.d/nix.sh"
fi

NIX_HOME_CONFIGURATION="${1:-linux-standard}"

nix build --no-link ".#homeConfigurations.$NIX_HOME_CONFIGURATION.activationPackage" --impure --extra-experimental-features "nix-command flakes"
NIX_HOME_MANAGER="$(nix path-info ".#homeConfigurations.$NIX_HOME_CONFIGURATION.activationPackage" --impure --extra-experimental-features "nix-command flakes")"/home-path/bin/home-manager
$NIX_HOME_MANAGER switch --flake ".#$NIX_HOME_CONFIGURATION" -b bak."$(date +"%Y%m%d")" --impure --extra-experimental-features "nix-command flakes"
