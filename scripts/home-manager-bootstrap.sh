#!/usr/bin/env sh
set -xe

if [ -f "$HOME/.nix-profile/etc/profile.d/nix.sh" ]; then
    . "$HOME/.nix-profile/etc/profile.d/nix.sh"
fi

# Install home-manager
nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
nix-channel --update

export NIX_PATH="$HOME/.nix-defexpr/channels:/nix/var/nix/profiles/per-user/root/channels${NIX_PATH:+:$NIX_PATH}"

nix-shell '<home-manager>' -A install

. "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"

# Build and activate home-manager configuration
# TODO: use .#linux-server
home-manager switch --flake ".#linux-desktop" -b bak."$(date +"%Y%m%d")" --extra-experimental-features "nix-command flakes"
