#!/usr/bin/env sh
set -xe

. lib/common.sh

source_nix

HME_HOME_CONFIGURATION="${1:-linux-base}"

HASH=$(echo $RANDOM | md5sum | head -c 5)

HME_TEMP_DIR="${TMPDIR:-/tmp}/hme.$HASH"
HME_HOME_DIR="${2:-"$HME_TEMP_DIR/home"}"

HME_HIST_DIR="${3:-"$HME_HOME_DIR/store/$(date +"%Y%m%dT%H%M%S")-$HASH"}"

HME_STORE_DIR="${4:-"${TMPDIR:-/tmp}/$HASH"}"

mkdir -p "$HME_TEMP_DIR"
mkdir -p "$HME_HIST_DIR"
rm -rf "$HME_STORE_DIR"
ln -snfF "$HME_HIST_DIR" "$HME_STORE_DIR"

HME_DEP_FILE="$HME_TEMP_DIR/hme.dep"
HME_CPIO_FILE="$HME_TEMP_DIR/hme.cpio"

ln -snfF "$HOME/.local/state/nix/profiles/profile" "$HOME/.nix-profile"
nix build --no-link ".#homeConfigurations.$HME_HOME_CONFIGURATION.activationPackage" --impure --extra-experimental-features "nix-command flakes" --accept-flake-config
HME_PACKAGE=$(nix path-info ".#homeConfigurations.$HME_HOME_CONFIGURATION.activationPackage" --impure --extra-experimental-features "nix-command flakes" --accept-flake-config)
nix-store -qR "$HME_PACKAGE" | xargs -L1 basename > "$HME_DEP_FILE.1"
HME_PACKAGE_2=$(readlink -f "$HOME/.nix-profile")
nix-store -qR "$HME_PACKAGE_2" | xargs -L1 basename > "$HME_DEP_FILE.2"
cat "$HME_DEP_FILE".* | sort | uniq > "$HME_DEP_FILE"
find /nix/store -depth -print | grep -Ff "$HME_DEP_FILE" | cpio -ov > "$HME_CPIO_FILE"
sed -i "s@/nix/store@$HME_STORE_DIR@g" "$HME_CPIO_FILE"
# mkdir -p "$HME_STORE_DIR"
cpio -idmv < "$HME_CPIO_FILE"
HME_PACKAGE_NEW=$(echo "$HME_PACKAGE" | sed "s@/nix/store@$HME_STORE_DIR@g")
find "$HME_PACKAGE_NEW/home-files/" -type d -exec chmod u+w {} \;
cp -av "$HME_PACKAGE_NEW/home-files/." "$HME_HOME_DIR/"
HME_PACKAGE_2_NEW=$(echo "$HME_PACKAGE_2" | sed "s@/nix/store@$HME_STORE_DIR@g")
ln -snfF "$HME_PACKAGE_2_NEW" "$HME_HOME_DIR/.nix-profile"

echo "$HME_TEMP_DIR"

# rm -rf "$HME_TEMP_DIR"
