#!/usr/bin/env python

import argparse
import enum
import logging
import subprocess
import sys
import typing


@enum.unique
class LogLevel(enum.Enum):
    Notset = "notset"
    Debug = "debug"
    Info = "info"
    Warning = "warning"
    Error = "error"
    Critical = "critical"

    def __str__(self):
        return self.value


@enum.unique
class BootMode(enum.Enum):
    Bios = "bios"
    Uefi = "uefi"

    def __str__(self):
        return self.value


@enum.unique
class PartitionMethod(enum.Enum):
    None_ = "none"
    Automatic = "automatic"
    Manual = "manual"

    def __str__(self):
        return self.value


def shell(
    cmd: str,
    input: typing.Optional[str] = None,
    stdout: bool = True,
    stderr: bool = True,
    dryrun: bool = False,
) -> typing.Tuple[int, typing.Optional[str], typing.Optional[str]]:
    if dryrun:
        logging.info(f"(DRYRUN) cmd=`{cmd}`")
        return 0, None, None
    else:
        logging.info(f"cmd=`{cmd}`")
        result = subprocess.run(
            cmd,
            shell=True,
            input=input.encode() if input else None,
            stdout=subprocess.PIPE if stdout else None,
            stderr=subprocess.PIPE if stderr else None,
        )
        return (
            result.returncode,
            result.stdout.decode().rstrip() if result.stdout else None,
            result.stderr.decode().rstrip() if result.stderr else None,
        )


class Prompt:
    @staticmethod
    def msgbox(title: str, text: str) -> typing.Tuple[int, typing.Optional[str]]:
        rc, _, stderr = shell(
            f'whiptail --title "{title}" --msgbox "{text}" 0 0', stdout=False
        )
        return rc, stderr

    @staticmethod
    def yesno(title: str, text: str) -> typing.Tuple[int, typing.Optional[str]]:
        rc, _, stderr = shell(
            f'whiptail --title "{title}" --yesno "{text}" 0 0', stdout=False
        )
        return rc, stderr

    @staticmethod
    def inputbox(
        title: str, text: str, default: typing.Optional[str] = None
    ) -> typing.Tuple[int, typing.Optional[str]]:
        rc, _, stderr = shell(
            f'whiptail --title "{title}" --inputbox "{text}" 0 0 "{default if default else str()}"',
            stdout=False,
        )
        return rc, stderr

    @staticmethod
    def passwordbox(title: str, text: str) -> typing.Tuple[int, typing.Optional[str]]:
        rc, _, stderr = shell(
            f'whiptail --title "{title}" --passwordbox "{text}" 8 0', stdout=False
        )
        return rc, stderr

    @staticmethod
    def menu(
        title: str,
        text: str,
        choices: typing.List[str],
        default: typing.Optional[str] = None,
    ) -> typing.Tuple[int, typing.Optional[str]]:
        choices = [f'"{c}" ""' for c in choices]
        rc, _, stderr = shell(
            f'whiptail --title "{title}" --default-item "{default if default else str()}" --menu "{text}" 0 0 0 {" ".join(choices)}',
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
    # # required_parser.add_argument('-t','--test',help='testing test',nargs='*',default=[],required=True)

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
        type=LogLevel,
        choices=list(LogLevel),
        default=LogLevel.Notset,
    )
    optional_parser.add_argument(
        "-b",
        "--boot-mode",
        help="boot mode to use",
        type=BootMode,
        choices=list(BootMode),
        default=BootMode.Uefi,
    )
    optional_parser.add_argument("-d", "--disk", help="disk to use for installation")
    optional_parser.add_argument(
        "-p",
        "--partition-method",
        help="partition method to use for disk",
        type=PartitionMethod,
        choices=list(PartitionMethod),
    )
    optional_parser.add_argument(
        "-c", "--crypt-partition", help="partition to use for LUKS container"
    )
    optional_parser.add_argument(
        "-r", "--crypt-passphrase", help="passphrase to use for LUKS container"
    )
    optional_parser.add_argument(
        "-y", "--crypt-name", help="name to use for LUKS container"
    )
    # optional_parser.add_argument("-o", "--boot", help="size of boot/efi partition")
    # optional_parser.add_argument("-s", "--swapfile", help="size of swapfile")
    # optional_parser.add_argument(
    #     "-r", "--root-password", help="password of the root account"
    # )
    # optional_parser.add_argument("-e", "--user-name", help="name of the user account")
    # optional_parser.add_argument(
    #     "-p", "--user-password", help="password of the user account"
    # )
    optional_parser.add_argument(
        "-n", "--dryrun", help="run in dryrun mode", action="store_true"
    )

    args = parser.parse_args()

    if args.log_level == LogLevel.Notset:
        args.log_level = logging.NOTSET
    elif args.log_level == LogLevel.Debug:
        args.log_level = logging.DEBUG
    elif args.log_level == LogLevel.Info:
        args.log_level = logging.INFO
    elif args.log_level == LogLevel.Warning:
        args.log_level = logging.WARNING
    elif args.log_level == LogLevel.Error:
        args.log_level = logging.ERROR
    elif args.log_level == LogLevel.Critical:
        args.log_level = logging.CRITICAL

    logging.basicConfig(
        stream=sys.stdout,
        format="%(asctime)s  [%(levelname)s]  (%(funcName)s)  %(message)s",
        datefmt="%Y-%m-%d %H:%M:%S.%e %z",
        level=args.log_level,
    )
    logging.debug(f"(PRE) args=`{args}`")

    if args.boot_mode == BootMode.Uefi:
        _, stdout, _ = shell(
            "find /sys/firmware/efi/efivars -print 2>/dev/null | wc -l"
        )
        if stdout == "0":
            Prompt.msgbox(
                "ARCHI (boot mode)",
                "Looks like we are not running in UEFI mode, please reboot and enable UEFI mode",
            )
            sys.exit(1)
    else:
        raise NotImplementedError()
    logging.debug(f"boot_mode=`{args.boot_mode}`")

    if not args.disk:
        _, stdout, _ = shell("lsblk -d -p -n -l -o NAME,SIZE -e 7,11")
        rc, stderr = Prompt.menu(
            "ARCHI (disk)",
            "Select disk device to use for installation",
            stdout.split("\n"),
        )
        if rc == 0:
            args.disk = stderr.split()[0]
        else:
            sys.exit(1)
    logging.debug(f"disk=`{args.disk}`")

    rc, _ = Prompt.yesno("ARCHI (disk)", f"Would you like to wipe {args.disk}?")
    if rc == 0:
        shell(f"wipefs -a -f {args.disk}", dryrun=True)

    if not args.partition_method:
        rc, stderr = Prompt.menu(
            "ARCHI (partition method)",
            f"Select partition method for {args.disk}",
            list(PartitionMethod),
            PartitionMethod.None_,
        )
        if rc == 0:
            args.partition_method = PartitionMethod(stderr)
        else:
            sys.exit(1)
    logging.debug(f"partition_method=`{args.partition_method}`")

    if args.partition_method == PartitionMethod.Automatic:
        shell(
            f'echo -e "g\nn\n\n\n+1G\nt\n1\nn\n\n\n\nt\n\n20\nw" | fdisk {args.disk}',
            dryrun=True,
        )
    elif args.partition_method == PartitionMethod.Manual:
        shell(f"cfdisk {args.disk}", dryrun=True)

    # if not args.crypt_partition:
    #     _, stdout, _ = shell(f"lsblk -p -n -l -o NAME,SIZE {args.disk} | tail -n +2")
    #     rc, stderr = Prompt.menu(
    #         "ARCHI (crypt partition)",
    #         "Select partition to create LUKS container",
    #         stdout.split("\n"),
    #     )
    #     if rc == 0:
    #         args.crypt_partition = stderr.split()[0]
    #     else:
    #         sys.exit(1)
    # logging.debug(f"crypt_partition=`{args.crypt_partition}`")

    # if not args.crypt_passphrase:
    #     rc1, stderr1 = Prompt.passwordbox(
    #         "ARCHI (crypt passphrase 1)", "Enter crypt passphrase"
    #     )
    #     rc2, stderr2 = Prompt.passwordbox(
    #         "ARCHI (crypt passphrase 2)", "Enter crypt passphrase again"
    #     )
    #     if rc1 == 0 and rc2 == 0 and stderr1 == stderr2:
    #         args.crypt_passphrase = stderr1
    #     else:
    #         sys.exit(1)
    # logging.debug(f"crypt_passphrase=`{args.crypt_passphrase}`")

    # if not args.crypt_name:
    #     pass
    # logging.debug(f"crypt_name=`{args.crypt_name}`")

    logging.debug(f"(POST) args=`{args}`")
