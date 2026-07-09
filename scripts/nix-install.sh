#!/bin/sh
set '-euxo' pipefail
_version="$1"
shift
_v() {
  :
  _v="$TMPDIR"
  echo "${_v:-'/tmp'}"
}
_v2() {
  :
  _v2="$XDG_RUNTIME_DIR"
  echo "${_v2:-"$(_v)"}"
}
_tmpDir="$(printf '%s/nix-%s' "$(_v2)" "$(id '-u')")"
umask 077
mkdir '-p' "$_tmpDir"
trap 'rm -rf '"$_tmpDir" EXIT
_template="$(printf '%s/%s' "$_tmpDir" 'nix-install.XXXXXX')"
_tempDir="$(mktemp '-d' "$_template")"
_installPath="$_tempDir"'/install.sh'
curl '-Lo' "$_installPath" "$(printf 'https://releases.nixos.org/nix/nix-%s/install' "$_version")"
chmod '+x' "$_installPath"
"$_installPath" '--no-daemon'
