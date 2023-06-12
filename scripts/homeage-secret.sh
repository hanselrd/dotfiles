#!/usr/bin/env sh
set -xe

if [ -f "$HOME/.nix-profile/etc/profile.d/nix.sh" ]; then
    . "$HOME/.nix-profile/etc/profile.d/nix.sh"
fi

find core/user/role/homeage/secrets -type f -not -name "*.age" -print -exec sh -c "age -a -R core/user/role/homeage/keys/1.age.pub {} > {}.age" \;
