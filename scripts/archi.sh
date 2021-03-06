#!/usr/bin/env bash

APP_NAME="archi"

prompt_msgbox() {
    whiptail \
        --title "$APP_NAME" \
        --msgbox "$1" 0 0
    # shellcheck disable=SC2181
    if [ "$?" != 0 ]; then
        return 1
    fi
    return 0
}

prompt_yesno() {
    whiptail \
        --title "$APP_NAME" \
        --yesno "$1" 0 0
    # shellcheck disable=SC2181
    if [ "$?" != 0 ]; then
        return 1
    fi
    return 0
}

prompt_inputbox() {
    result=$(whiptail \
                --title "$APP_NAME" \
                --inputbox "$1" 0 0 "$2" \
                3>&1 1>&2 2>&3)
    # shellcheck disable=SC2181
    if [ "$?" != 0 ]; then
        return 1
    fi
    echo "${result%%\ *}"
    return 0
}

prompt_passwordbox() {
    result=$(whiptail \
                --title "$APP_NAME" \
                --passwordbox "$1" 8 0 "" \
                3>&1 1>&2 2>&3)
    # shellcheck disable=SC2181
    if [ "$?" != 0 ]; then
        return 1
    fi
    echo "${result%%\ *}"
    return 0
}

prompt_menu() {
    options=()
    IFS_ORIG=$IFS
    IFS=$'\n'
    for item in $2; do
        options+=("$item" "")
    done
    IFS=$IFS_ORIG
    result=$(whiptail \
        --title "$APP_NAME" \
        --nocancel \
        --default-item "$3" \
        --menu "$1" 0 0 0 \
        "${options[@]}" \
        3>&1 1>&2 2>&3)
    # shellcheck disable=SC2181
    if [ "$?" != 0 ]; then
        return 1
    fi
    echo "${result%%\ *}"
    return 0
}

## MAIN

if [ "$(find /sys/firmware/efi/efivars -print 2>/dev/null | wc -l)" == 0 ]; then
    prompt_msgbox "Looks like we are not running in UEFI mode, please reboot and enable UEFI mode"
    exit 1
fi

device=$(prompt_menu "Select disk device to use for installation" "$(lsblk -d -p -n -l -o NAME,SIZE -e 7,11)")
# device=$(prompt_menu "Select disk device to use for installation" "/dev/sdb")

if (prompt_yesno "Would you like to wipe and partition ${device}?"); then
    wipefs -a -f "$device"
    prompt_msgbox "Wiped $device"

    partition_method=$(prompt_menu "How would you like to partition ${device}?" $'Automatic\nManual')
    case "$partition_method" in
        Automatic)
            echo -e "g\nn\n\n\n+1G\nt\n1\nn\n\n\n\nt\n\n20\nw" | fdisk "$device"
            ;;
        Manual)
            cfdisk "$device"
            ;;
    esac
    prompt_msgbox "Partitioned $device"
fi

if (prompt_yesno "Would you like to create a LUKS container on ${device}?"); then
    crypt_partition=$(prompt_menu "Select partition to create LUKS container on" "$(lsblk -p -n -l -o NAME,SIZE "$device" | tail -n +2)")

    passphrase=$(prompt_passwordbox "Enter passphrase")
    passphrase_again=$(prompt_passwordbox "Enter passphrase again")
    while [ "$passphrase" != "$passphrase_again" ]; do
        prompt_msgbox "Passphrases do not match"
        passphrase=$(prompt_passwordbox "Enter passphrase")
        passphrase_again=$(prompt_passwordbox "Enter passphrase again")
    done

    cryptname=$(prompt_inputbox "Enter name of LUKS container" "crypt")

    echo -n "$passphrase" | cryptsetup luksFormat --type luks1 "$crypt_partition" -
    echo -n "$passphrase" | cryptsetup luksOpen "$crypt_partition" "$cryptname" -
elif (prompt_yesno "Would you like to open a LUKS container on ${device}?"); then
    crypt_partition=$(prompt_menu "Select partition to open LUKS container on" "$(lsblk -p -n -l -o NAME,SIZE "$device" | tail -n +2)")

    passphrase=$(prompt_passwordbox "Enter passphrase")
    cryptname=$(prompt_inputbox "Enter name of LUKS container" "crypt")

    echo -n "$passphrase" | cryptsetup luksOpen "$crypt_partition" "$cryptname" -
fi

efi_partition=$(prompt_menu "Select EFI partition" "$(lsblk -p -n -l -o NAME,SIZE "$device" | tail -n +2)")
if (prompt_yesno "Would you like to format $efi_partition using EFI?"); then
    mkfs.fat -F32 "$efi_partition"
fi

btrfs_partition=$(prompt_menu "Select BTRFS partition" "$(lsblk -p -n -l -o NAME,SIZE "$device" | tail -n +2)")
if (prompt_yesno "Would you like to format $btrfs_partition using BTRFS?"); then
    mkfs.btrfs -f "$btrfs_partition"
fi

if (prompt_yesno "Would you like to create BTRFS subvolumes on ${btrfs_partition}?"); then
    mount "$btrfs_partition" /mnt
    btrfs subvolume create /mnt/@
    btrfs subvolume create /mnt/@home
    btrfs subvolume create /mnt/@shared
    btrfs subvolume create /mnt/@swap
    btrfs subvolume create /mnt/@keys
    umount /mnt
    prompt_msgbox "Created BTRFS subvolumes on $btrfs_partition"
fi

mount -o compress=lzo,subvol=@ "$btrfs_partition" /mnt
mkdir -p /mnt/{efi,home,.shared,.swap,.keys}
mount "$efi_partition" /mnt/efi
mount -o compress=lzo,subvol=@home "$btrfs_partition" /mnt/home
mount -o compress=lzo,subvol=@shared "$btrfs_partition" /mnt/.shared
mount -o compress=lzo,subvol=@swap "$btrfs_partition" /mnt/.swap
mount -o compress=lzo,subvol=@keys "$btrfs_partition" /mnt/.keys
chmod 777 /mnt/.shared
prompt_msgbox "Mounted partitions on $device"

timedatectl set-ntp true
prompt_msgbox "Updated the system clock"

if (prompt_yesno "Would you like to create a swapfile?"); then
    truncate -s 0 /mnt/.swap/swapfile
    chattr +C /mnt/.swap/swapfile
    if [ "$(findmnt -no FSTYPE -T /mnt/.swap)" == "btrfs" ]; then
        btrfs property set /mnt/.swap/swapfile compression none
    fi

    memory=$(prompt_inputbox "Enter amount of memory to allocate for swapfile" "$(free -g | grep "Mem" | awk '{printf("%d\n", $2+1)}')G")

    fallocate -l "$memory" /mnt/.swap/swapfile
    chmod 600 /mnt/.swap/swapfile
    mkswap /mnt/.swap/swapfile
    swapon /mnt/.swap/swapfile
    prompt_msgbox "Created swapfile"
fi

pacman -S reflector --noconfirm
reflector --country="United States" --age=12 --protocol=https --sort=rate --save=/etc/pacman.d/mirrorlist
pacman -Syyy --noconfirm
prompt_msgbox "Selected pacman mirrors"

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
prompt_msgbox "Installed essential packages"

genfstab -U /mnt >> /mnt/etc/fstab
prompt_msgbox "Generated Fstab"

timezone=$(prompt_menu "Select time zone" "$(find /usr/share/zoneinfo -print)" "/usr/share/zoneinfo/America/New_York")
arch-chroot /mnt ln -sf "$timezone" /etc/localtime
arch-chroot /mnt hwclock --systohc

locale=$(prompt_menu "Select locale" "$(grep "^[[:alnum:]]\|^#[[:alnum:]]" /etc/locale.gen | sed 's/#//g; s/ *$//g')" "en_US.UTF-8 UTF-8")
arch-chroot /mnt sed -i "/$locale/s/^#//g" /etc/locale.gen
arch-chroot /mnt locale-gen
arch-chroot /mnt bash -c "echo 'LANG=$locale' >> /etc/locale.conf"
arch-chroot /mnt bash -c "echo 'LC_COLLATE=C' >> /etc/locale.conf"

hostname=$(prompt_inputbox "Enter your hostname" "arch")
arch-chroot /mnt bash -c "echo '$hostname' >> /etc/hostname"
# shellcheck disable=SC2129
arch-chroot /mnt bash -c "echo -e '127.0.0.1\tlocalhost' >> /etc/hosts"
arch-chroot /mnt bash -c "echo -e '::1\t\tlocalhost' >> /etc/hosts"
arch-chroot /mnt bash -c "echo -e '127.0.1.1\t$hostname.localdomain\t$hostname' >> /etc/hosts"
arch-chroot /mnt systemctl enable NetworkManager

if [ "$cryptname" != "" ]; then
    arch-chroot /mnt dd if=/dev/random of=/.keys/"$cryptname".keyfile iflag=fullblock bs=512 count=8
    arch-chroot /mnt chmod 000 /.keys/"$cryptname".keyfile
    arch-chroot /mnt bash -c "echo -n '$passphrase' | cryptsetup luksAddKey $crypt_partition /.keys/$cryptname.keyfile -"
    prompt_msgbox "Created LUKS key for $crypt_partition"
fi

arch-chroot /mnt sed -i 's/^BINARIES=(.*)/BINARIES=(\/usr\/bin\/btrfs)/g' /etc/mkinitcpio.conf
if [ "$cryptname" != "" ]; then
    arch-chroot /mnt sed -i "s/^FILES=(.*)/FILES=(\/.keys\/$cryptname.keyfile)/g" /etc/mkinitcpio.conf
    arch-chroot /mnt sed -i 's/^HOOKS=(.*)/HOOKS=(base systemd autodetect keyboard modconf block sd-encrypt filesystems fsck)/g' /etc/mkinitcpio.conf
else
    arch-chroot /mnt sed -i 's/^HOOKS=(.*)/HOOKS=(base systemd autodetect keyboard modconf block filesystems fsck)/g' /etc/mkinitcpio.conf
fi
arch-chroot /mnt mkinitcpio -P
prompt_msgbox "Generated initramfs"

root_password=$(prompt_passwordbox "Enter root password")
root_password_again=$(prompt_passwordbox "Enter root password again")
while [ "$root_password" != "$root_password_again" ]; do
    prompt_msgbox "Passwords do not match"
    root_password=$(prompt_passwordbox "Enter root password")
    root_password_again=$(prompt_passwordbox "Enter root password again")
done
arch-chroot /mnt bash -c "echo 'root:$root_password' | chpasswd"

user_name=$(prompt_inputbox "Enter user name")
user_password=$(prompt_passwordbox "Enter $user_name password")
user_password_again=$(prompt_passwordbox "Enter $user_name password again")
while [ "$user_password" != "$user_password_again" ]; do
    prompt_msgbox "Passwords do not match"
    user_password=$(prompt_passwordbox "Enter $user_name password")
    user_password_again=$(prompt_passwordbox "Enter $user_name password again")
done
arch-chroot /mnt useradd -mG wheel "$user_name"
arch-chroot /mnt bash -c "echo '$user_name:$user_password' | chpasswd"

arch-chroot /mnt sed -i '/%wheel ALL=(ALL) ALL/s/^# //g' /etc/sudoers
prompt_msgbox "Updated sudoers for wheel group"

arch-chroot /mnt sed -i '/GRUB_DEFAULT=/s/0/"Arch Linux, with Linux linux"/g' /etc/default/grub
arch-chroot /mnt sed -i '/GRUB_CMDLINE_LINUX_DEFAULT=/s/".*"/"loglevel=3"/g' /etc/default/grub
if [ "$cryptname" != "" ]; then
    uuid=$(lsblk -no UUID "$crypt_partition" | head -n 1)
    arch-chroot /mnt sed -i '/GRUB_CMDLINE_LINUX=/s/".*"/"rd.luks.name='"$uuid"'='"$cryptname"' rd.luks.key='"$uuid"'=\/.keys\/'"$cryptname"'.keyfile"/g' /etc/default/grub
    arch-chroot /mnt sed -i '/GRUB_ENABLE_CRYPTODISK=y/s/^#//g' /etc/default/grub
fi
arch-chroot /mnt bash -c "echo -e '\nGRUB_DISABLE_SUBMENU=y' >> /etc/default/grub"
arch-chroot /mnt grub-install --target=x86_64-efi --efi-directory=/efi --bootloader-id=GRUB --recheck
arch-chroot /mnt grub-mkconfig -o /boot/grub/grub.cfg
prompt_msgbox "Installed and configured boot loader"

arch-chroot /mnt sed -i '/Color/s/^#//g' /etc/pacman.conf
arch-chroot /mnt sed -i '/CheckSpace/s/^#//g' /etc/pacman.conf
arch-chroot /mnt sed -i '/VerbosePkgLists/s/^#//g' /etc/pacman.conf
prompt_msgbox "Set pacman options"

arch-chroot /mnt bash -c "cd /.shared && sudo -u $user_name git clone https://aur.archlinux.org/yay.git"
arch-chroot /mnt bash -c "cd /.shared/yay && sudo -u $user_name makepkg -sirc --noconfirm"
prompt_msgbox "Installed yay AUR helper"

arch-chroot /mnt bash -c "cd /.shared && sudo -u $user_name git clone https://hanselrd@github.com/hanselrd/dotfiles.git"
prompt_msgbox "Cloned dotfiles"

arch-chroot /mnt snapper --no-dbus --config=root create-config /
arch-chroot /mnt snapper --no-dbus --config=root create --description="Initial"
arch-chroot /mnt snapper --no-dbus --config=home create-config /home
arch-chroot /mnt snapper --no-dbus --config=home create --description="Initial"
prompt_msgbox "Configured snapper"

# umount -R /mnt
# if [ "$cryptname" != "" ]; then
#     cryptsetup luksClose "$cryptname" 2>/dev/null
# fi
