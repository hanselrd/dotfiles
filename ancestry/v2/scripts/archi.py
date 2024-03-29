#!/usr/bin/env python

import argparse
import dataclasses
import logging
import shutil
import subprocess
import sys
import time
import typing


def shell(
    cmd: str,
    stdout: bool = True,
    stderr: bool = True,
    dryrun: bool = False,
) -> typing.Tuple[int, typing.Optional[str], typing.Optional[str]]:
    if dryrun:
        logging.info(f"(DRYRUN) cmd=`{repr(cmd)}`")
        return 0, None, None
    else:
        logging.info(f"cmd=`{repr(cmd)}`")
        process = subprocess.Popen(
            cmd,
            universal_newlines=True,
            shell=True,
            stdout=subprocess.PIPE if stdout else None,
            stderr=subprocess.PIPE if stderr else None,
        )
        if stdout:
            start = time.time()
            while True:
                try:
                    stdout, stderr = process.communicate(timeout=1)
                    break
                except subprocess.TimeoutExpired:
                    elapsed = int(time.time() - start)
                    if elapsed >= 1:
                        columns, _ = shutil.get_terminal_size()
                        progress = "=" * (elapsed % ((columns // 2) - 15))
                        print(
                            f"<{progress} {elapsed}s {progress}>".center(columns),
                            end="\r",
                            flush=True,
                        )
                    continue
        else:
            stdout, stderr = process.communicate()
        if stdout:
            logging.info(f"  stdout=`{repr(stdout)}`")
        if stderr:
            logging.info(f"  stderr=`{repr(stderr)}`")
        return (
            process.returncode,
            stdout.strip() if stdout else None,
            stderr.strip() if stderr else None,
        )


class Constants:
    BACKTITLE = "ARCHI - a fast, opinionated Arch Linux Installer"


class Prompt:
    @staticmethod
    def msgbox(title: str, text: str) -> typing.Tuple[int, typing.Optional[str]]:
        rc, _, stderr = shell(
            f"whiptail --backtitle '{Constants.BACKTITLE}' --title '{title}' --msgbox '{text}' 0 0",
            stdout=False,
        )
        return rc, stderr

    @staticmethod
    def yesno(title: str, text: str) -> typing.Tuple[int, typing.Optional[str]]:
        rc, _, stderr = shell(
            f"whiptail --backtitle '{Constants.BACKTITLE}' --title '{title}' --yesno '{text}' 0 0",
            stdout=False,
        )
        return rc, stderr

    @staticmethod
    def inputbox(
        title: str, text: str, default: typing.Optional[str] = None
    ) -> typing.Tuple[int, typing.Optional[str]]:
        rc, _, stderr = shell(
            f"whiptail --backtitle '{Constants.BACKTITLE}' --title '{title}' --inputbox '{text}' 0 0 '{default if default else str()}'",
            stdout=False,
        )
        return rc, stderr

    @staticmethod
    def passwordbox(title: str, text: str) -> typing.Tuple[int, typing.Optional[str]]:
        rc, _, stderr = shell(
            f"whiptail --backtitle '{Constants.BACKTITLE}' --title '{title}' --passwordbox '{text}' 8 0",
            stdout=False,
        )
        return rc, stderr

    @dataclasses.dataclass(frozen=True)
    class Item:
        tag: str
        description: str = ""
        state: bool = False

    @staticmethod
    def menu(
        title: str,
        text: str,
        items: typing.List[Item],
        default: typing.Optional[str] = None,
    ) -> typing.Tuple[int, typing.Optional[str]]:
        items = [f"'{i.tag}' ' {i.description}'" for i in items]
        rc, _, stderr = shell(
            f"whiptail --backtitle '{Constants.BACKTITLE}' --title '{title}' --default-item '{default if default else str()}' --menu '{text}' 0 0 0 {' '.join(items)}",
            stdout=False,
        )
        return rc, stderr

    @staticmethod
    def checklist(
        title: str, text: str, items: typing.List[Item]
    ) -> typing.Tuple[int, typing.Optional[typing.List[str]]]:
        items = [
            f"'{i.tag}' ' {i.description}' '{'on' if i.state == True else 'off'}'"
            for i in items
        ]
        rc, _, stderr = shell(
            f"whiptail --backtitle '{Constants.BACKTITLE}' --title '{title}' --checklist '{text}' 0 0 0 {' '.join(items)}",
            stdout=False,
        )
        return rc, [s.strip('"') for s in stderr.split('" "')] if stderr else None

    @staticmethod
    def radiolist(
        title: str, text: str, items: typing.List[Item]
    ) -> typing.Tuple[int, typing.Optional[str]]:
        items = [
            f"'{i.tag}' ' {i.description}' '{'on' if i.state == True else 'off'}'"
            for i in items
        ]
        rc, _, stderr = shell(
            f"whiptail --backtitle '{Constants.BACKTITLE}' --title '{title}' --radiolist '{text}' 0 0 0 {' '.join(items)}",
            stdout=False,
        )
        return rc, stderr


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Fast, opinionated Arch Linux installer",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
        add_help=False,
    )

    # positional_parser = parser.add_argument_group("positional arguments")

    # required_parser = parser.add_argument_group("required arguments")

    optional_parser = parser.add_argument_group("optional arguments")
    optional_parser.add_argument(
        "-h",
        "--help",
        help="show this help message and exit",
        action="help",
        default=argparse.SUPPRESS,
    )
    optional_parser.add_argument(
        "-l",
        "--log-level",
        help="set logging level",
        choices=[
            logging.NOTSET,
            logging.DEBUG,
            logging.INFO,
            logging.WARNING,
            logging.ERROR,
            logging.CRITICAL,
        ],
        default=logging.DEBUG,
    )
    optional_parser.add_argument(
        "-n", "--dryrun", help="run in dryrun mode", action="store_true"
    )

    args = parser.parse_args()

    logging.basicConfig(
        stream=sys.stdout,
        format="%(asctime)s  %(levelname)-8s  |  (%(funcName)s)  %(message)s",
        datefmt="%Y-%m-%d %H:%M:%S.%e %z",
        level=args.log_level,
    )

    Prompt.msgbox(
        "ARCHI",
        "Welcome to the fast, opinionated Arch Linux installer. If you have never installed Arch Linux before manually then this installer is NOT for you. This installer only supports UEFI and will install a fully encrypted BTRFS Arch Linux system.",
    )

    # Check UEFI support
    _, stdout, _ = shell("find /sys/firmware/efi/efivars -print 2>/dev/null | wc -l")
    if stdout == "0":
        Prompt.msgbox(
            "ARCHI",
            "Looks like we are not running in UEFI mode, please reboot and enable UEFI mode",
        )
        sys.exit(1)

    # Select disk
    _, stdout, _ = shell("lsblk -d -p -n -l -o NAME,SIZE -e 7,11")
    items = [
        Prompt.Item(tag=s[0], description=" ".join(s[1:]))
        for s in [s.split() for s in stdout.split("\n")]
    ]
    rc, stderr = Prompt.menu(
        "ARCHI", "Select disk device to use for installation", items
    )
    if rc == 0:
        DISK = stderr
    else:
        sys.exit(1)

    # Wipe disk
    rc, _ = Prompt.yesno("ARCHI", f"Would you like to wipe {DISK}?")
    if rc == 0:
        shell(f"wipefs -a -f {DISK}", dryrun=args.dryrun)

    # Partition disk
    rc, _ = Prompt.yesno("ARCHI", f"Would you like to partition {DISK}?")
    if rc == 0:
        items = [
            Prompt.Item(tag="automatic"),
            Prompt.Item(tag="manual", description="(using cfdisk)"),
            Prompt.Item(tag="none"),
        ]
        rc, stderr = Prompt.menu("ARCHI", f"Select partition method for {DISK}", items)
        if rc == 0:
            if stderr == "automatic":
                shell(
                    f"echo -e 'g\nn\n\n\n+1G\nt\n1\nn\n\n\n\nt\n\n20\nw' | fdisk {DISK}",
                    dryrun=args.dryrun,
                )
            elif stderr == "manual":
                shell(f"cfdisk {DISK}", stdout=False, dryrun=args.dryrun)
        else:
            sys.exit(1)

    # Select LUKS container partition
    items = [
        Prompt.Item(tag="open an existing one"),
        Prompt.Item(tag="create a new one"),
    ]
    rc, stderr = Prompt.menu(
        "ARCHI",
        f"Would you like to open an existing LUKS container or create a new one on {DISK}?",
        items,
    )
    if rc == 0:
        LUKS_CREATE = True if stderr == "create a new one" else False
        _, stdout, _ = shell(f"lsblk -p -n -l -o NAME,SIZE {DISK}")
        items = [
            Prompt.Item(tag=s[0], description=" ".join(s[1:]))
            for s in [s.split() for s in stdout.split("\n")]
        ]
        rc, stderr = Prompt.menu(
            "ARCHI", f"Select LUKS container partition on {DISK}", items
        )
        if rc == 0:
            LUKS_PARTITION = stderr
            while True:
                rc1, stderr1 = Prompt.passwordbox(
                    "ARCHI", "Enter LUKS container passphrase"
                )
                rc2, stderr2 = Prompt.passwordbox(
                    "ARCHI", "Enter LUKS container passphrase again"
                )
                if rc1 == 0 and rc2 == 0:
                    if stderr1 == stderr2:
                        LUKS_PASSPHRASE = stderr1
                        rc, stderr = Prompt.inputbox(
                            "ARCHI", "Enter LUKS container name", "cryptbtrfs"
                        )
                        if rc == 0:
                            LUKS_NAME = stderr
                            if LUKS_CREATE:
                                shell(
                                    f"echo -n '{LUKS_PASSPHRASE}' | cryptsetup luksFormat --type luks1 {LUKS_PARTITION} -",
                                    dryrun=args.dryrun,
                                )
                            shell(
                                f"echo -n '{LUKS_PASSPHRASE}' | cryptsetup luksOpen {LUKS_PARTITION} {LUKS_NAME} -",
                                dryrun=args.dryrun,
                            )
                        else:
                            sys.exit(1)
                        break
                    else:
                        Prompt.msgbox(
                            "ARCHI",
                            "LUKS container passphrases do not match, please try again",
                        )
                else:
                    sys.exit(1)
        else:
            sys.exit(1)
    else:
        sys.exit(1)

    # Select EFI partition
    _, stdout, _ = shell(f"lsblk -p -n -l -o NAME,SIZE {DISK}")
    items = [
        Prompt.Item(tag=s[0], description=" ".join(s[1:]))
        for s in [s.split() for s in stdout.split("\n")]
    ]
    rc, stderr = Prompt.menu("ARCHI", f"Select EFI partition on {DISK}", items)
    if rc == 0:
        EFI_PARTITION = stderr
        rc, _ = Prompt.yesno(
            "ARCHI", f"Would you like to reformat {EFI_PARTITION} using EFI?"
        )
        if rc == 0:
            shell(f"mkfs.fat -F32 {EFI_PARTITION}", dryrun=args.dryrun)
    else:
        sys.exit(1)

    # Select BTRFS partition
    _, stdout, _ = shell(f"lsblk -p -n -l -o NAME,SIZE {DISK}")
    items = [
        Prompt.Item(tag=s[0], description=" ".join(s[1:]))
        for s in [s.split() for s in stdout.split("\n")]
    ]
    rc, stderr = Prompt.menu("ARCHI", f"Select BTRFS partition on {DISK}", items)
    if rc == 0:
        BTRFS_PARTITION = stderr
        rc, _ = Prompt.yesno(
            "ARCHI", f"Would you like to reformat {BTRFS_PARTITION} using BTRFS?"
        )
        if rc == 0:
            shell(f"mkfs.btrfs -f {BTRFS_PARTITION}", dryrun=args.dryrun)
        rc, _ = Prompt.yesno(
            "ARCHI", f"Would you like to create BTRFS subvolumes on {BTRFS_PARTITION}?"
        )
        if rc == 0:
            shell(f"mount {BTRFS_PARTITION} /mnt", dryrun=args.dryrun)
            shell(f"btrfs subvolume create /mnt/@", dryrun=args.dryrun)
            shell(f"btrfs subvolume create /mnt/@home", dryrun=args.dryrun)
            shell(f"btrfs subvolume create /mnt/@shared", dryrun=args.dryrun)
            shell(f"btrfs subvolume create /mnt/@swap", dryrun=args.dryrun)
            shell(f"btrfs subvolume create /mnt/@keys", dryrun=args.dryrun)
            shell(f"umount /mnt", dryrun=args.dryrun)
    else:
        sys.exit(1)

    # Mount partitions and BTRFS subvolumes
    shell(
        f"mount -o compress-force=zstd:1,subvol=@ {BTRFS_PARTITION} /mnt",
        dryrun=args.dryrun,
    )
    shell("mkdir -p /mnt/{efi,home,.shared,.swap,.keys}", dryrun=args.dryrun)
    shell(f"mount {EFI_PARTITION} /mnt/efi", dryrun=args.dryrun)
    shell(
        f"mount -o compress-force=zstd:1,subvol=@home {BTRFS_PARTITION} /mnt/home",
        dryrun=args.dryrun,
    )
    shell(
        f"mount -o compress-force=zstd:1,subvol=@shared {BTRFS_PARTITION} /mnt/.shared",
        dryrun=args.dryrun,
    )
    shell("chmod 777 /mnt/.shared", dryrun=args.dryrun)
    shell(
        f"mount -o compress-force=zstd:1,subvol=@swap {BTRFS_PARTITION} /mnt/.swap",
        dryrun=args.dryrun,
    )
    shell(
        f"mount -o compress-force=zstd:1,subvol=@keys {BTRFS_PARTITION} /mnt/.keys",
        dryrun=args.dryrun,
    )

    # Set system clock
    shell("timedatectl set-ntp true", dryrun=args.dryrun)

    # Create swapfile
    rc, _ = Prompt.yesno("ARCHI", "Would you like to create a swapfile?")
    if rc == 0:
        shell("truncate -s 0 /mnt/.swap/swapfile", dryrun=args.dryrun)
        shell("chattr +C /mnt/.swap/swapfile", dryrun=args.dryrun)
        shell(
            "btrfs property set /mnt/.swap/swapfile compression none",
            dryrun=args.dryrun,
        )
        _, stdout, _ = shell("free -g | grep 'Mem' | awk '{print $2+1}'")
        rc, stderr = Prompt.inputbox(
            "ARCHI", "Enter amount of memory to allocate for swapfile", f"{stdout}G"
        )
        if rc == 0:
            SWAPFILE_SIZE = stderr
            shell(
                f"fallocate -l {SWAPFILE_SIZE} /mnt/.swap/swapfile", dryrun=args.dryrun
            )
            shell("chmod 600 /mnt/.swap/swapfile", dryrun=args.dryrun)
            shell("mkswap /mnt/.swap/swapfile", dryrun=args.dryrun)
            shell("swapon /mnt/.swap/swapfile", dryrun=args.dryrun)
        else:
            sys.exit(1)

    # Update pacman mirrors
    # shell("pacman -S reflector --noconfirm", dryrun=args.dryrun)
    _, stdout, _ = shell(
        'reflector --list-countries | tail -n +3 | rev | awk \'{out=""; for(i=3;i<=NF;++i){out=out" "$i}; print $1,$2,out}\' | rev'
    )
    items = [
        Prompt.Item(tag=s[0], description=" ".join(s[1:]))
        for s in [s.split("  ") for s in stdout.split("\n")]
    ]
    items = [
        dataclasses.replace(i, state=True) if i.tag == "United States" else i
        for i in items
    ]
    rc, stderr = Prompt.checklist("ARCHI", "Select pacman mirrors", items)
    if rc == 0:
        PACMAN_MIRRORS = stderr
        shell(
            f"reflector --country='{','.join(PACMAN_MIRRORS)}' --age=12 --protocol=https --sort=rate --save=/etc/pacman.d/mirrorlist",
            dryrun=args.dryrun,
        )
        shell("pacman -Syyy --noconfirm", dryrun=args.dryrun)
    else:
        sys.exit(1)

    # Install essential packages
    items = [
        Prompt.Item(tag="amd-ucode"),
        Prompt.Item(tag="ansible", state=True),
        Prompt.Item(tag="base", state=True),
        Prompt.Item(tag="base-devel", state=True),
        Prompt.Item(tag="btrfs-progs", state=True),
        Prompt.Item(tag="cronie", state=True),
        Prompt.Item(tag="dhall", state=True),
        Prompt.Item(tag="dhall-yaml", state=True),
        Prompt.Item(tag="efibootmgr", state=True),
        Prompt.Item(tag="git", state=True),
        Prompt.Item(tag="grub", state=True),
        Prompt.Item(tag="grub-btrfs", state=True),
        Prompt.Item(tag="intel-ucode", state=True),
        Prompt.Item(tag="linux", state=True),
        Prompt.Item(tag="linux-firmware", state=True),
        Prompt.Item(tag="linux-headers", state=True),
        Prompt.Item(tag="linux-lts", state=True),
        Prompt.Item(tag="linux-lts-headers", state=True),
        Prompt.Item(tag="network-manager-applet", state=True),
        Prompt.Item(tag="networkmanager", state=True),
        Prompt.Item(tag="os-prober", state=True),
        Prompt.Item(tag="snap-pac", state=True),
        Prompt.Item(tag="snapper", state=True),
        Prompt.Item(tag="vim", state=True),
    ]
    rc, stderr = Prompt.checklist("ARCHI", "Select essential packages", items)
    if rc == 0:
        PACKAGES = stderr
        shell(f"pacstrap /mnt {' '.join(PACKAGES)}", dryrun=args.dryrun)
    else:
        sys.exit(1)

    # Generate fstab
    shell("genfstab -U /mnt >> /mnt/etc/fstab", dryrun=args.dryrun)

    # Select time zone
    _, stdout, _ = shell("find /usr/share/zoneinfo -print")
    items = [
        Prompt.Item(tag=s[0], description=" ".join(s[1:]))
        for s in [s.split() for s in stdout.split("\n")]
    ]
    rc, stderr = Prompt.menu(
        "ARCHI",
        "Select time zone",
        items,
        "/usr/share/zoneinfo/America/New_York",
    )
    if rc == 0:
        TIME_ZONE = stderr
        shell(f"arch-chroot /mnt ln -sf {TIME_ZONE} /etc/localtime", dryrun=args.dryrun)
        shell(f"arch-chroot /mnt hwclock --systohc", dryrun=args.dryrun)
    else:
        sys.exit(1)

    # Select locale
    _, stdout, _ = shell(
        "grep '^[[:alnum:]]\|^#[[:alnum:]]' /etc/locale.gen | sed 's/#//g; s/ *$//g'"
    )
    items = [
        Prompt.Item(tag=s[0], description=" ".join(s[1:]))
        for s in [s.split() for s in stdout.split("\n")]
    ]
    rc, stderr = Prompt.menu("ARCHI", "Select locale", items, "en_US.UTF-8")
    if rc == 0:
        LOCALE = stderr
        shell(
            f"arch-chroot /mnt sed -i '/#{LOCALE}/s/^#//g' /etc/locale.gen",
            dryrun=args.dryrun,
        )
        shell(f"arch-chroot /mnt locale-gen", dryrun=args.dryrun)
        shell(
            f"arch-chroot /mnt bash -c 'echo \"LANG={LOCALE}\" >> /etc/locale.conf'",
            dryrun=args.dryrun,
        )
        shell(
            f"arch-chroot /mnt bash -c 'echo \"LC_COLLATE=C\" >> /etc/locale.conf'",
            dryrun=args.dryrun,
        )
    else:
        sys.exit(1)

    # Set hostname
    rc, stderr = Prompt.inputbox("ARCHI", "Enter hostname", "arch")
    if rc == 0:
        HOSTNAME = stderr
        shell(
            f"arch-chroot /mnt bash -c 'echo \"{HOSTNAME}\" >> /etc/hostname'",
            dryrun=args.dryrun,
        )
        shell(
            f"arch-chroot /mnt bash -c 'echo -e \"127.0.0.1\tlocalhost\" >> /etc/hosts'",
            dryrun=args.dryrun,
        )
        shell(
            f"arch-chroot /mnt bash -c 'echo -e \"::1\t\tlocalhost\" >> /etc/hosts'",
            dryrun=args.dryrun,
        )
        shell(
            f"arch-chroot /mnt bash -c 'echo -e \"127.0.0.1\t{HOSTNAME}.domain\t{HOSTNAME}\" >> /etc/hosts'",
            dryrun=args.dryrun,
        )
        shell(f"arch-chroot /mnt systemctl enable NetworkManager", dryrun=args.dryrun)
    else:
        sys.exit(1)

    # Create LUKS container key
    shell(
        f"arch-chroot /mnt dd if=/dev/random of=/.keys/{LUKS_NAME}.keyfile iflag=fullblock bs=512 count=8",
        dryrun=args.dryrun,
    )
    shell(f"arch-chroot /mnt chmod 000 /.keys/{LUKS_NAME}.keyfile", dryrun=args.dryrun)
    shell(
        f"arch-chroot /mnt bash -c 'echo -n \"{LUKS_PASSPHRASE}\" | cryptsetup luksAddKey {LUKS_PARTITION} /.keys/{LUKS_NAME}.keyfile -'",
        dryrun=args.dryrun,
    )

    # Generate initramfs
    shell(
        f"arch-chroot /mnt sed -i 's/^BINARIES=(.*)/BINARIES=(\/usr\/bin\/btrfs)/g' /etc/mkinitcpio.conf",
        dryrun=args.dryrun,
    )
    shell(
        f"arch-chroot /mnt sed -i 's/^FILES=(.*)/FILES=(\/.keys\/{LUKS_NAME}.keyfile)/g' /etc/mkinitcpio.conf",
        dryrun=args.dryrun,
    )
    shell(
        f"arch-chroot /mnt sed -i 's/^HOOKS=(.*)/HOOKS=(base systemd autodetect keyboard modconf block sd-encrypt filesystems fsck)/g' /etc/mkinitcpio.conf",
        dryrun=args.dryrun,
    )
    shell(f"arch-chroot /mnt mkinitcpio -P", dryrun=args.dryrun)

    # Set root password
    while True:
        rc1, stderr1 = Prompt.passwordbox("ARCHI", "Enter root password")
        rc2, stderr2 = Prompt.passwordbox("ARCHI", "Enter root password again")
        if rc1 == 0 and rc2 == 0:
            if stderr1 == stderr2:
                ROOT_PASSWORD = stderr1
                shell(
                    f"arch-chroot /mnt bash -c 'echo \"root:{ROOT_PASSWORD}\" | chpasswd'",
                    dryrun=args.dryrun,
                )
                break
            else:
                Prompt.msgbox("ARCHI", "Root passwords do not match, please try again")
        else:
            sys.exit(1)

    # Create user account
    rc, stderr = Prompt.inputbox("ARCHI", "Enter user name")
    if rc == 0:
        USER_NAME = stderr
        while True:
            rc1, stderr1 = Prompt.passwordbox("ARCHI", f"Enter {USER_NAME} password")
            rc2, stderr2 = Prompt.passwordbox(
                "ARCHI", f"Enter {USER_NAME} password again"
            )
            if rc1 == 0 and rc2 == 0:
                if stderr1 == stderr2:
                    USER_PASSWORD = stderr1
                    shell(
                        f"arch-chroot /mnt useradd -mG wheel {USER_NAME}",
                        dryrun=args.dryrun,
                    )
                    shell(
                        f"arch-chroot /mnt bash -c 'echo \"{USER_NAME}:{USER_PASSWORD}\" | chpasswd'",
                        dryrun=args.dryrun,
                    )
                    break
                else:
                    Prompt.msgbox(
                        "ARCHI", "User passwords do not match, please try again"
                    )
            else:
                sys.exit(1)
    else:
        sys.exit(1)

    # Update sudoers
    shell(
        f"arch-chroot /mnt sed -i '/%wheel ALL=(ALL) ALL/s/^# //g' /etc/sudoers",
        dryrun=args.dryrun,
    )

    # Configure boot loader
    shell(
        f"arch-chroot /mnt sed -i '/GRUB_DEFAULT=/s/0/\"Arch Linux, with Linux linux\"/g' /etc/default/grub",
        dryrun=args.dryrun,
    )
    shell(
        f'arch-chroot /mnt sed -i \'/GRUB_CMDLINE_LINUX_DEFAULT=/s/".*"/"loglevel=3"/g\' /etc/default/grub',
        dryrun=args.dryrun,
    )
    _, stdout, _ = shell(
        f"lsblk -no UUID,TYPE {LUKS_PARTITION} | grep 'part' | cut -d' ' -f 1"
    )
    shell(
        f'arch-chroot /mnt sed -i \'/GRUB_CMDLINE_LINUX=/s/".*"/"rd.luks.name={stdout}={LUKS_NAME} rd.luks.key={stdout}=\/.keys\/{LUKS_NAME}.keyfile"/g\' /etc/default/grub',
        dryrun=args.dryrun,
    )
    shell(
        f"arch-chroot /mnt sed -i '/GRUB_ENABLE_CRYPTODISK=y/s/^#//g' /etc/default/grub",
        dryrun=args.dryrun,
    )
    shell(
        f"arch-chroot /mnt sed -i '/GRUB_DISABLE_SUBMENU=y/s/^#//g' /etc/default/grub",
        dryrun=args.dryrun,
    )
    shell(
        f"arch-chroot /mnt grub-install --target=x86_64-efi --efi-directory=/efi --bootloader-id=GRUB --recheck",
        dryrun=args.dryrun,
    )
    shell(f"arch-chroot /mnt grub-mkconfig -o /boot/grub/grub.cfg", dryrun=args.dryrun)

    # Set pacman options
    shell(
        f"arch-chroot /mnt sed -i '/Color/s/^#//g' /etc/pacman.conf", dryrun=args.dryrun
    )
    shell(
        f"arch-chroot /mnt sed -i '/CheckSpace/s/^#//g' /etc/pacman.conf",
        dryrun=args.dryrun,
    )
    shell(
        f"arch-chroot /mnt sed -i '/VerbosePkgLists/s/^#//g' /etc/pacman.conf",
        dryrun=args.dryrun,
    )
    shell(
        f"arch-chroot /mnt sed -i '/VerbosePkgLists/s/$/\nILoveCandy/g' /etc/pacman.conf",
        dryrun=args.dryrun,
    )
    shell(
        f"arch-chroot /mnt sed -i '/ParallelDownloads/s/^#//g' /etc/pacman.conf",
        dryrun=args.dryrun,
    )
    shell(
        "arch-chroot /mnt sed -i '/\[multilib\]/ {N; s/#//g}' /etc/pacman.conf",
        dryrun=args.dryrun,
    )
    shell(f"arch-chroot /mnt pacman -Syyy --noconfirm", dryrun=args.dryrun)

    # Install yay AUR helper
    shell(
        f"arch-chroot /mnt bash -c 'cd /.shared && sudo -u {USER_NAME} git clone https://aur.archlinux.org/yay.git'",
        dryrun=args.dryrun,
    )
    shell(
        f"arch-chroot /mnt bash -c 'cd /.shared/yay && sudo -u {USER_NAME} makepkg -sirc --noconfirm'",
        dryrun=args.dryrun,
    )

    # Clone dotfiles
    shell(
        f"arch-chroot /mnt bash -c 'cd /.shared && sudo -u {USER_NAME} git clone https://hanselrd@github.com/hanselrd/dotfiles.git'",
        dryrun=args.dryrun,
    )

    # Configure snapper
    shell(
        f"arch-chroot /mnt snapper --no-dbus --config=root create-config /",
        dryrun=args.dryrun,
    )
    shell(
        f"arch-chroot /mnt snapper --no-dbus --config=root create --description='Initial'",
        dryrun=args.dryrun,
    )
    shell(
        f"arch-chroot /mnt snapper --no-dbus --config=home create-config /home",
        dryrun=args.dryrun,
    )
    shell(
        f"arch-chroot /mnt snapper --no-dbus --config=home create --description='Initial'",
        dryrun=args.dryrun,
    )
