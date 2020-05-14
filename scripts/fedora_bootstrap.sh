#!/usr/bin/env sh

# Assumes the following setup

# Mount       Device        Type
# -----------------------------------------------
# (/boot/efi) /dev/sdX1     EFI System Partition
# (/.btrfs)   /dev/sdX2     LUKS (btrfs)
#     (/)           @
#     (/home)       @home
#     (/.shared)    @shared
#     (/.swap)      @swap
#     (/.keys)      @keys

# Open LUKS
cryptsetup luksOpen /dev/sda2 cryptbtrfs

# Mount partitions
mount -o subvol=@ /dev/sda2 /mnt
mount /dev/sda1 /mnt/boot/efi
mount /dev/sda2 /mnt/.btrfs
mount -o subvol=@home /dev/sda2 /mnt/home
mount -o subvol=@shared /dev/sda2 /mnt/.shared
mount -o subvol=@swap /dev/sda2 /mnt/.swap
mount -o subvol=@keys /dev/sda2 /mnt/.keys
mount -o bind /dev /mnt/dev
mount -o bind /proc /mnt/proc
mount -o bind /sys /mnt/sys
mount -o bind /run /mnt/run
mount -o bind /tmp /mnt/tmp

# Change to filesystem mounted under /mnt
chroot /mnt

# Create swapfile
truncate -s 0 /.swap/swapfile
chattr +C /.swap/swapfile
btrfs property set /.swap/swapfile compression none
fallocate -l 4G /.swap/swapfile
chmod 600 /.swap/swapfile
mkswap /.swap/swapfile
swapon /.swap/swapfile
echo "/.swap/swapfile                           none                    swap    defaults 0 0" >> /etc/fstab
systemctl daemon-reload

# Create keys
dd if=/dev/random of=/.keys/cryptbtrfs.keyfile iflag=fullblock bs=512 count=4
chmod 000 /.keys/cryptbtrfs.keyfile
cryptsetup luksDump /dev/sda2
cryptsetup luksAddKey /dev/sda2 /.keys/cryptbtrfs.keyfile
cryptsetup luksDump /dev/sda2
echo 'install_items+="/.keys/cryptbtrfs.keyfile"' >> /etc/dracut.conf
sed -i 's/none/\/.keys\/cryptbtrfs.keyfile/g' /etc/crypttab
dracut --force

# Check if keys were added to initramfs
# mkdir -p /tmp/i
# cd /tmp/i
# file /boot/initramfs-$(uname -r).img
# cpio -ivdF /boot/initramfs-$(uname -r).img
# dd if=/boot/initramfs-$(uname -r).img of=initramfs-$(uname -r).img bs=512 skip=N
# zcat initramfs-$(uname -r).img | cpio -ivd
# ll .keys
# cd ..

# Install snapper
dnf install snapper python3-dnf-plugin-snapper
sudo snapper --config=root create-config /
sudo snapper --config=root create --description="Initial"
sudo snapper --config=home create-config /home
sudo snapper --config=home create --description="Initial"

# Reinstall grub
dnf reinstall grub2-efi shim grub2-tools
echo "GRUB_ENABLE_CRYPTODISK=y" >> /etc/default/grub
grub2-mkconfig -o /boot/efi/EFI/fedora/grub.cfg

# Install git and ansible
dnf install git ansible -y
