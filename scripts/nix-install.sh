#!/bin/sh
set '-euxo' pipefail
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
curl '-Lo' "$_installPath" 'https://nixos.org/nix/install'
chmod '+x' "$_installPath"
"$_installPath" '--no-daemon'
