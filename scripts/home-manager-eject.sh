#!/usr/bin/env sh
set -xe

if [ -f "$HOME/.nix-profile/etc/profile.d/nix.sh" ]; then
    . "$HOME/.nix-profile/etc/profile.d/nix.sh"
fi

HME_HOME_CONFIGURATION="${1:-linux-server}"

HME_TEMP_DIR=$(mktemp -d "${TMPDIR:-/tmp}/hme.XXXXXX")
HME_HOME_DIR="${2:-"$HME_TEMP_DIR/home"}"

HME_STORE_DIR="${3:-$(mktemp -d "${TMPDIR:-/tmp}/XXXXX")}"

HME_DEP_FILE="$HME_TEMP_DIR/hme.dep"
HME_CPIO_FILE="$HME_TEMP_DIR/hme.cpio"

nix build --no-link ".#homeConfigurations.$HME_HOME_CONFIGURATION.activationPackage" --impure --extra-experimental-features "nix-command flakes"
HME_PACKAGE=$(nix path-info ".#homeConfigurations.$HME_HOME_CONFIGURATION.activationPackage" --impure --extra-experimental-features "nix-command flakes")
nix-store -qR "$HME_PACKAGE" | xargs -L1 basename > "$HME_DEP_FILE.1"
HME_PACKAGE_2=$(readlink -f "$HOME/.nix-profile")
nix-store -qR "$HME_PACKAGE_2" | xargs -L1 basename > "$HME_DEP_FILE.2"
cat $HME_DEP_FILE.* | sort | uniq > "$HME_DEP_FILE"
find /nix/store -depth -print | grep -Ff "$HME_DEP_FILE" | cpio -ov > "$HME_CPIO_FILE"
sed -i "s@/nix/store@$HME_STORE_DIR@g" "$HME_CPIO_FILE"
mkdir -p "$HME_STORE_DIR"
cpio -idmv < "$HME_CPIO_FILE"
HME_PACKAGE_NEW=$(echo "$HME_PACKAGE" | sed "s@/nix/store@$HME_STORE_DIR@g")
find "$HME_PACKAGE_NEW/home-files/" -type d -exec chmod u+w {} \;
cp -av "$HME_PACKAGE_NEW/home-files/." "$HME_HOME_DIR/"
HME_PACKAGE_2_NEW=$(echo "$HME_PACKAGE_2" | sed "s@/nix/store@$HME_STORE_DIR@g")
ln -snfF "$HME_PACKAGE_2_NEW" "$HME_HOME_DIR/.nix-profile"

echo "$HME_TEMP_DIR"

# rm -rf "$HME_TEMP_DIR"
