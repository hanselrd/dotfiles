#!/usr/bin/env bash

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

echo "Open LUKS container on /dev/sda2"
cryptsetup luksOpen /dev/sda2 cryptbtrfs

echo "Mount partitions"
mount -o compress=lzo,subvol=@ /dev/mapper/cryptbtrfs /mnt
mkdir -p /mnt/{efi,home,.shared,.swap,.keys}
mount /dev/sda1 /mnt/efi
mount -o compress=lzo,subvol=@home /dev/mapper/cryptbtrfs /mnt/home
mount -o compress=lzo,subvol=@shared /dev/mapper/cryptbtrfs /mnt/.shared
mount -o compress=lzo,subvol=@swap /dev/mapper/cryptbtrfs /mnt/.swap
mount -o compress=lzo,subvol=@keys /dev/mapper/cryptbtrfs /mnt/.keys
chmod 777 /mnt/.shared

echo "Verify the boot mode"
ls /sys/firmware/efi/efivars

echo "Connect to the internet"
wifi-menu

echo "Update the system clock"
timedatectl set-ntp true

echo "Create swapfile"
truncate -s 0 /mnt/.swap/swapfile
chattr +C /mnt/.swap/swapfile
if [ "$(findmnt -no FSTYPE -T /mnt/.swap)" == "btrfs" ]; then
    btrfs property set /mnt/.swap/swapfile compression none
fi
fallocate -l $(free -g | grep "Mem" | awk '{printf("%d\n", $2+1)}')G /mnt/.swap/swapfile
chmod 600 /mnt/.swap/swapfile
mkswap /mnt/.swap/swapfile
swapon /mnt/.swap/swapfile

echo "Select the mirrors"
pacman -S reflector --noconfirm
reflector --country="United States" --age=12 --protocol=https --sort=rate --save=/etc/pacman.d/mirrorlist
pacman -Syyy --noconfirm

echo "Install essential packages"
pacstrap /mnt \
    ansible \
    base \
    base-devel \
    btrfs-progs \
    efibootmgr \
    git \
    grub \
    grub-btrfs \
    intel-ucode \
    linux \
    linux-firmware \
    linux-headers \
    linux-lts \
    linux-lts-headers \
    network-manager-applet \
    networkmanager \
    os-prober \
    snap-pac \
    snapper \
    vim

echo "Fstab"
genfstab -U /mnt >> /mnt/etc/fstab

cat <<EOF >> /mnt/bootstrap.sh
echo "Time zone"
ln -sf /usr/share/zoneinfo/America/New_York /etc/localtime
hwclock --systohc

echo "Localization"
sed -i 's/#en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/g' /etc/locale.gen
sed -i 's/#en_US ISO-8859-1/en_US ISO-8859-1/g' /etc/locale.gen
locale-gen
echo "LANG=en_US.UTF-8" >> /etc/locale.conf

echo "Network configuration"
echo "arch" >> /etc/hostname
printf "127.0.0.1\tlocalhost\n" >> /etc/hosts
printf "::1\t\tlocalhost\n" >> /etc/hosts
printf "127.0.1.1\tarch.localdomain\tarch\n" >> /etc/hosts
systemctl enable NetworkManager

echo "Create LUKS key"
dd if=/dev/random of=/.keys/cryptbtrfs.keyfile iflag=fullblock bs=512 count=8
chmod 000 /.keys/cryptbtrfs.keyfile
cryptsetup luksAddKey /dev/sda2 /.keys/cryptbtrfs.keyfile

echo "Initramfs"
sed -i 's/^BINARIES=(.*)/BINARIES=(\/usr\/bin\/btrfs)/g' /etc/mkinitcpio.conf
sed -i 's/^FILES=(.*)/FILES=(\/.keys\/cryptbtrfs.keyfile)/g' /etc/mkinitcpio.conf
sed -i 's/^HOOKS=(.*)/HOOKS=(base systemd autodetect keyboard modconf block sd-encrypt filesystems fsck)/g' /etc/mkinitcpio.conf
mkinitcpio -P

echo "Root password"
passwd

echo "User password"
useradd -mG wheel delacruz
passwd delacruz

echo "Update sudoers for wheel group"
sed -i 's/# %wheel ALL=(ALL) ALL/%wheel ALL=(ALL) ALL/g' /etc/sudoers

echo "Snapper"
snapper --no-dbus --config=root create-config /
snapper --no-dbus --config=root create --description="Initial"
snapper --no-dbus --config=home create-config /home
snapper --no-dbus --config=home create --description="Initial"

echo "Boot loader"
uuid=$(lsblk -no UUID /dev/sda2 | head -n 1)
sed -i 's/loglevel=3 quiet/loglevel=3/g' /etc/default/grub
sed -i 's/GRUB_CMDLINE_LINUX=""/GRUB_CMDLINE_LINUX="rd.luks.name=$uuid=cryptbtrfs rd.luks.key=$uuid=/.keys/cryptbtrfs.keyfile"/g' /etc/default/grub
sed -i 's/#GRUB_ENABLE_CRYPTODISK=y/GRUB_ENABLE_CRYPTODISK=y/g' /etc/default/grub
echo "GRUB_DISABLE_SUBMENU=y" >> /etc/default/grub
grub-install --target=x86_64-efi --efi-directory=/efi --bootloader-id=GRUB --recheck
grub-mkconfig -o /boot/grub/grub.cfg

echo "Set Pacman Options"
sed -i 's/#Color/Color/g' /etc/pacman.conf
sed -i 's/#CheckSpace/CheckSpace/g' /etc/pacman.conf
sed -i 's/#VerbosePkgLists/VerbosePkgLists/g' /etc/pacman.conf

echo "Exit chroot"
exit
EOF

echo "Chroot"
arch-chroot /mnt /bootstrap.sh

echo "Cleanup chroot bootstrap"
rm -rf /mnt/bootstrap.sh

# Install yay
# cd /.shared
# git clone https://aur.archlinux.org/yay.git
# cd yay
# makepkg -si --noconfirm

# Update system
# sudo pacman -Syu
# sudo pacman -Fy

# Bootstrap dotfiles
# cd /.shared
# git clone https://hanselrd@github.com/hanselrd/dotfiles.git
# cd dotfiles
# Add roles/bootstrap/vars/main.yml variables
# sudo ansible-playbook localhost.yml
