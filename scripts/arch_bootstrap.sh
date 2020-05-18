#!/usr/bin/env bash

device="/dev/sda"
efi_device="$device"1
crypt_device="$device"2
user=""

echo "Verify the boot mode"
ls -lh /sys/firmware/efi/efivars

echo "Wipe device $device"
wipefs -a -f "$device"

echo "Partition device $device"
echo -e "g\nn\n\n\n+1G\nt\n1\nn\n\n\n\nt\n\n20\nw" | fdisk "$device"

echo "Format $efi_device with FAT32"
mkfs.fat -F32 "$efi_device"

echo "Create LUKS container on $crypt_device"
cryptsetup luksFormat --type luks1 "$crypt_device"
cryptsetup luksOpen "$crypt_device" cryptbtrfs

echo "Format /dev/mapper/cryptbtrfs with BTRFS"
mkfs.btrfs /dev/mapper/cryptbtrfs

echo "Create BTRFS subvolumes"
mount /dev/mapper/cryptbtrfs /mnt
btrfs subvolume create /mnt/@
btrfs subvolume create /mnt/@home
btrfs subvolume create /mnt/@shared
btrfs subvolume create /mnt/@swap
btrfs subvolume create /mnt/@keys
umount /mnt

echo "Mount partitions"
mount -o compress=lzo,subvol=@ /dev/mapper/cryptbtrfs /mnt
mkdir -p /mnt/{efi,home,.shared,.swap,.keys}
mount "$efi_device" /mnt/efi
mount -o compress=lzo,subvol=@home /dev/mapper/cryptbtrfs /mnt/home
mount -o compress=lzo,subvol=@shared /dev/mapper/cryptbtrfs /mnt/.shared
mount -o compress=lzo,subvol=@swap /dev/mapper/cryptbtrfs /mnt/.swap
mount -o compress=lzo,subvol=@keys /dev/mapper/cryptbtrfs /mnt/.keys
chmod 777 /mnt/.shared

echo "Update the system clock"
timedatectl set-ntp true

echo "Create swapfile"
truncate -s 0 /mnt/.swap/swapfile
chattr +C /mnt/.swap/swapfile
if [ "$(findmnt -no FSTYPE -T /mnt/.swap)" == "btrfs" ]; then
    btrfs property set /mnt/.swap/swapfile compression none
fi
fallocate -l "$(free -g | grep "Mem" | awk '{printf("%d\n", $2+1)}')G" /mnt/.swap/swapfile
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

uuid=$(lsblk -no UUID "$crypt_device" | head -n 1)

cat <<EOF >> /mnt/bootstrap.sh
echo "Time zone"
ln -sf /usr/share/zoneinfo/America/New_York /etc/localtime
hwclock --systohc

echo "Localization"
sed -i '/en_US.UTF-8 UTF-8/s/^#//g' /etc/locale.gen
sed -i '/en_US ISO-8859-1/s/^#//g' /etc/locale.gen
locale-gen
echo "LANG=en_US.UTF-8" >> /etc/locale.conf

echo "Network configuration"
echo "arch" >> /etc/hostname
echo -e "127.0.0.1\tlocalhost" >> /etc/hosts
echo -e "::1\t\tlocalhost" >> /etc/hosts
echo -e "127.0.1.1\tarch.localdomain\tarch" >> /etc/hosts
systemctl enable NetworkManager

echo "Create LUKS key"
dd if=/dev/random of=/.keys/cryptbtrfs.keyfile iflag=fullblock bs=512 count=8
chmod 000 /.keys/cryptbtrfs.keyfile
cryptsetup luksAddKey "$crypt_device" /.keys/cryptbtrfs.keyfile

echo "Initramfs"
sed -i 's/^BINARIES=(.*)/BINARIES=(\/usr\/bin\/btrfs)/g' /etc/mkinitcpio.conf
sed -i 's/^FILES=(.*)/FILES=(\/.keys\/cryptbtrfs.keyfile)/g' /etc/mkinitcpio.conf
sed -i 's/^HOOKS=(.*)/HOOKS=(base systemd autodetect keyboard modconf block sd-encrypt filesystems fsck)/g' /etc/mkinitcpio.conf
mkinitcpio -P

echo "Root password"
passwd

echo "User password"
useradd -mG wheel "$user"
passwd "$user"

echo "Update sudoers for wheel group"
sed -i '/%wheel ALL=(ALL) ALL/s/^# //g' /etc/sudoers

echo "Snapper"
snapper --no-dbus --config=root create-config /
snapper --no-dbus --config=root create --description="Initial"
snapper --no-dbus --config=home create-config /home
snapper --no-dbus --config=home create --description="Initial"

echo "Boot loader"
sed -i '/GRUB_CMDLINE_LINUX_DEFAULT=/s/".*"/"loglevel=3"/g' /etc/default/grub
sed -i '/GRUB_CMDLINE_LINUX=/s/".*"/"rd.luks.name=$uuid=cryptbtrfs rd.luks.key=$uuid=\/.keys\/cryptbtrfs.keyfile"/g' /etc/default/grub
sed -i '/GRUB_ENABLE_CRYPTODISK=y/s/^#//g' /etc/default/grub
echo "GRUB_DISABLE_SUBMENU=y" >> /etc/default/grub
grub-install --target=x86_64-efi --efi-directory=/efi --bootloader-id=GRUB --recheck
grub-mkconfig -o /boot/grub/grub.cfg

echo "Set Pacman Options"
sed -i '/Color/s/^#//g' /etc/pacman.conf
sed -i '/CheckSpace/s/^#//g' /etc/pacman.conf
sed -i '/VerbosePkgLists/s/^#//g' /etc/pacman.conf

echo "Change to user"
sudo su "$user"

echo "Install yay"
cd /.shared || exit
git clone https://aur.archlinux.org/yay.git
cd yay || exit
makepkg -si --noconfirm

echo "Clone dotfiles"
cd ..
git clone https://hanselrd@github.com/hanselrd/dotfiles.git

echo "Exit chroot"
exit
EOF

echo "Chroot"
chmod +x /mnt/bootstrap.sh
arch-chroot /mnt /bootstrap.sh

echo "Cleanup chroot bootstrap"
rm -rf /mnt/bootstrap.sh
