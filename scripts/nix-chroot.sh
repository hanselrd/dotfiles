#!/bin/sh
set '-euxo' pipefail
_type="$1"
shift
_dir="$1"
shift
_installDir="$HOME"'/.nix-chroot'
mkdir '-p' "$_installDir"
case "$_type" in bindmnt)
  :
  sudo mkdir '-pm' 0755 '/nix' "$_dir"
  sudo chown "$USER" '/nix' "$_dir"
  sudo mount '--bind' "$_dir" '/nix'
  :
  ;;
'nix-user-chroot')
  :
  _nixUserChrootPath="$_installDir"'/nix-user-chroot'
  curl '-Lo' "$_nixUserChrootPath" 'https://github.com/nix-community/nix-user-chroot/releases/download/2.1.1/nix-user-chroot-bin-2.1.1-x86_64-unknown-linux-musl'
  chmod '+x' "$_nixUserChrootPath"
  mkdir '-p' "$_dir"
  "$_nixUserChrootPath" "$_dir" bash '-l'
  :
  ;;
proot)
  :
  _prootPath="$_installDir"'/proot'
  curl '-Lo' "$_prootPath" 'https://proot.gitlab.io/proot/bin/proot'
  chmod '+x' "$_prootPath"
  mkdir '-p' "$_dir"
  "$_prootPath" '-b' "$_dir"':/nix' bash
  :
  ;;
*)
  :
  exit 1
  ;;
esac
