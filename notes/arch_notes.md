<!-- #!/usr/bin/env sh -->

# Verify the boot mode
ls /sys/firmware/efi/efivars

# Connect to the internet
ip link
ping archlinux.org
wifi-menu
ping archlinux.org

# Update the system clock
timedatectl set-ntp true

# LVM on LUKS
## Preparing the disk
cryptsetup luksFormat /dev/sda1

cryptsetup luksOpen /dev/sda1 cryptlvm

## Preparing the logical volumes
pvcreate /dev/mapper/cryptlvm

vgcreate my_vg /dev/mapper/cryptlvm

lvcreate -L 4G my_vg -n swap
lvcreate -L 50G my_vg -n root
lvcreate -l 100%FREE my_vg -n home

mkfs.ext4 /dev/my_vg/root
mkfs.ext4 /dev/my_vg/home
mkswap /dev/my_vg/swap

mount /dev/my_vg/root /mnt
mkdir -p /mnt/home
mount /dev/my_vg/home /mnt/home
swapon /dev/my_vg/swap

## Preparing the boot partition
mkfs.fat -F32 /dev/sdb1

mkdir -p /mnt/efi

mount /dev/sd1 /mnt/efi

# Miscellaneous
mount /dev/mapper/cryptroot /mnt
btrfs subvolume create /mnt/@arch
btrfs subvolume create /mnt/@arch-home
btrfs subvolume create /mnt/@arch-snapshots
btrfs subvolume create /mnt/@arch-swap
umount /mnt
mount /dev/mapper/cryptroot /mnt/.btrfs
mount -o compress=zstd,subvol=@arch /dev/mapper/cryptroot /mnt
mount -o compress=zstd,subvol=@arch-home /dev/mapper/cryptroot /mnt/home
mount -o compress=zstd,subvol=@arch-snapshots /dev/mapper/cryptroot /mnt/.snapshots
mount -o compress=zstd,subvol=@arch-swap /dev/mapper/cryptroot /mnt/.swap
truncate -s 0 /mnt/.swap/swapfile
chattr +C /mnt/.swap/swapfile
btrfs property set /mnt/.swap/swapfile
fallocate -l 4G /mnt/.swap/swapfile
chmod 600 /mnt/.swap/swapfile
mkswap /mnt/.swap/swapfile
swapon /mnt/.swap/swapfile
<!-- echo "/.swap/swapfile none swap defaults 0 0" >> /etc/fstab -->
<!-- swapoff /.swap/swapfile -->
<!-- rm -rf /.swap/swapfile -->
git clone https://aur.archlinux.org/yay.git
cd yay
makepkg -si
