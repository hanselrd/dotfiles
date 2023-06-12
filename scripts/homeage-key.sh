#!/usr/bin/env sh
set -xe

if [ -f "$HOME/.nix-profile/etc/profile.d/nix.sh" ]; then
    . "$HOME/.nix-profile/etc/profile.d/nix.sh"
fi

age -d -i core/user/role/homeage/keys/0.age core/user/role/homeage/keys/1.age > "$HOME/.keys/2.age"
