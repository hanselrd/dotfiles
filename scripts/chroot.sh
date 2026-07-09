#!/bin/sh
set '-euxo' pipefail
_installDir="$HOME"'/.bootstrap'
_nixUserChrootPath="$_installDir"'/nix-user-chroot'
_prootPath="$_installDir"'/proot'
curl '-Lo' "$_nixUserChrootPath" 'https://github.com/nix-community/nix-user-chroot/releases/download/2.1.1/nix-user-chroot-bin-2.1.1-x86_64-unknown-linux-musl'
chmod '+x' "$_nixUserChrootPath"
curl '-Lo' "$_prootPath" 'https://proot.gitlab.io/proot/bin/proot'
chmod '+x' "$_prootPath"
