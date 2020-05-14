#!/usr/bin/env sh

# Assumes the following setup

# Mount       Device        Type
# -----------------------------------------------
# (/efi)      /dev/sdX1     EFI System Partition
#             /dev/sdX2     LUKS (btrfs)
#     (/)           @
#     (/home)       @home
#     (/.shared)    @shared
#     (/.swap)      @swap
#     (/.keys)      @keys

# Open LUKS
cryptsetup luksOpen /dev/sda2 cryptbtrfs

# Mount partitions
mount -o compress=lzo,subvol=@ /dev/mapper/cryptbtrfs /mnt
mkdir -p /mnt/{efi,home,.shared,.swap,.keys}
chmod 777 /mnt/.shared
mount /dev/sda1 /mnt/efi
mount -o compress=lzo,subvol=@home /dev/mapper/cryptbtrfs /mnt/home
mount -o compress=lzo,subvol=@shared /dev/mapper/cryptbtrfs /mnt/.shared
mount -o compress=lzo,subvol=@swap /dev/mapper/cryptbtrfs /mnt/.swap
mount -o compress=lzo,subvol=@keys /dev/mapper/cryptbtrfs /mnt/.keys

# Verify the boot mode
ls /sys/firmware/efi/efivars

# Connect to the internet
wifi-menu

# Update the system clock
timedatectl set-ntp true

# Create swapfile
truncate -s 0 /mnt/.swap/swapfile
chattr +C /mnt/.swap/swapfile
if [ "$(findmnt -no FSTYPE -T /mnt/.swap)" == "btrfs" ]; then
    btrfs property set /mnt/.swap/swapfile compression none
fi
fallocate -l $(free -g | head -n 2 | tail -n 1 | awk '{printf("%d\n", $2+1)}')G /mnt/.swap/swapfile
chmod 600 /mnt/.swap/swapfile
mkswap /mnt/.swap/swapfile
swapon /mnt/.swap/swapfile

# Select the mirrors
pacman -S reflector --noconfirm
reflector --country "United States" --age 12 --sort rate --save /etc/pacman.d/mirrorlist
pacman -Syyy --noconfirm

# Install essential packages
pacstrap /mnt base base-devel linux linux-headers linux-firmware vim

# Fstab
genfstab -U /mnt >> /mnt/etc/fstab

# Chroot
arch-chroot /mnt

##############################################################################
##### from inside /mnt (arch-chroot)
###############################################################################

# Install yay
cd /.shared
git clone https://aur.archlinux.org/yay.git
cd yay
makepkg -si --noconfirm

# # Change to filesystem mounted under /mnt
# chroot /mnt

# # Create swapfile
# truncate -s 0 /.swap/swapfile
# chattr +C /.swap/swapfile
# btrfs property set /.swap/swapfile compression none
# fallocate -l 4G /.swap/swapfile
# chmod 600 /.swap/swapfile
# mkswap /.swap/swapfile
# swapon /.swap/swapfile
# echo "/.swap/swapfile                           none                    swap    defaults 0 0" >> /etc/fstab
# systemctl daemon-reload

# # Create keys
# dd if=/dev/random of=/.keys/cryptbtrfs.keyfile iflag=fullblock bs=512 count=4
# chmod 000 /.keys/cryptbtrfs.keyfile
# cryptsetup luksDump /dev/sda2
# cryptsetup luksAddKey /dev/sda2 /.keys/cryptbtrfs.keyfile
# cryptsetup luksDump /dev/sda2
# echo 'install_items+="/.keys/cryptbtrfs.keyfile"' >> /etc/dracut.conf
# sed -i 's/none/\/.keys\/cryptbtrfs.keyfile/g' /etc/crypttab
# dracut --force

# # Check if keys were added to initramfs
# # mkdir -p /tmp/i
# # cd /tmp/i
# # file /boot/initramfs-$(uname -r).img
# # cpio -ivdF /boot/initramfs-$(uname -r).img
# # dd if=/boot/initramfs-$(uname -r).img of=initramfs-$(uname -r).img bs=512 skip=N
# # zcat initramfs-$(uname -r).img | cpio -ivd
# # ll .keys
# # cd ..

# # Install snapper
# dnf install snapper python3-dnf-plugin-snapper
# snapper --config=root create-config /
# snapper --config=root create --description="Initial"
# snapper --config=home create-config /home
# snapper --config=home create --description="Initial"

# # Reinstall grub
# dnf reinstall grub2-efi shim grub2-tools
# echo "GRUB_ENABLE_CRYPTODISK=y" >> /etc/default/grub
# grub2-mkconfig -o /boot/efi/EFI/fedora/grub.cfg

# # Install git and ansible
# dnf install git ansible -y

# # Set up mounts
# # Set up dnf fastestmirror and deltarpm
# # Set up grub
# # Install git and ansible
# # Reboot
# # Bootstrap
# # Change /.swap/swapfile size
