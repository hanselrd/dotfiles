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


def shell(
    cmd: str,
    input: str = None,
    stdout: bool = True,
    stderr: bool = True,
    dryrun: bool = False,
) -> typing.Tuple[int, str, str]:
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
    def msgbox(title: str, text: str) -> typing.Tuple[int, str]:
        rc, _, stderr = shell(
            f'whiptail --title "{title}" --msgbox "{text}" 0 0', stdout=False
        )
        return rc, stderr

    @staticmethod
    def yesno(title: str, text: str) -> typing.Tuple[int, str]:
        rc, _, stderr = shell(
            f'whiptail --title "{title}" --yesno "{text}" 0 0', stdout=False
        )
        return rc, stderr

    @staticmethod
    def inputbox(title: str, text: str, default: str = None) -> typing.Tuple[int, str]:
        rc, _, stderr = shell(
            f'whiptail --title "{title}" --inputbox "{text}" 0 0 "{default if default else str()}"',
            stdout=False,
        )
        return rc, stderr

    @staticmethod
    def passwordbox(title: str, text: str) -> typing.Tuple[int, str]:
        rc, _, stderr = shell(
            f'whiptail --title "{title}" --passwordbox "{text}" 8 0', stdout=False
        )
        return rc, stderr

    @staticmethod
    def menu(
        title: str, text: str, choices: typing.List[str], default: str = None
    ) -> typing.Tuple[int, str]:
        choices = [f'"{c}" ""' for c in choices]
        rc, _, stderr = shell(
            f'whiptail --title "{title}" --default-item "{default if default else str()}" --menu "{text}" 0 0 0 {" ".join(choices)}',
            stdout=False,
        )
        return rc, stderr


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        formatter_class=argparse.ArgumentDefaultsHelpFormatter, add_help=False
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
    optional_mutex_parser = optional_parser.add_mutually_exclusive_group()
    optional_parser.add_argument(
        "-l",
        "--log-level",
        help="set logging level",
        type=LogLevel,
        choices=list(LogLevel),
        default=LogLevel.Notset,
    )
    optional_mutex_parser.add_argument(
        "-u", "--uefi", help="use UEFI mode", action="store_true"
    )
    optional_mutex_parser.add_argument(
        "-b", "--bios", help="use BIOS mode", action="store_true"
    )
    optional_parser.add_argument("-d", "--disk", help="disk to use for installation")
    optional_parser.add_argument("-o", "--boot", help="size of boot/efi partition")
    optional_parser.add_argument("-s", "--swapfile", help="size of swapfile")
    optional_parser.add_argument(
        "-r", "--root-password", help="password of the root account"
    )
    optional_parser.add_argument("-e", "--user-name", help="name of the user account")
    optional_parser.add_argument(
        "-p", "--user-password", help="password of the user account"
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
        format="%(asctime)s  [%(levelname)s]  %(message)s",
        datefmt="%Y-%m-%d %H:%M:%S.%e %z",
        level=args.log_level,
    )
    logging.debug(f"args=`{args}`")

    # Check for UEFI
    if args.uefi or not args.bios:
        _, stdout, _ = shell(
            "find /sys/firmware/efi/efivars -print 2>/dev/null | wc -l"
        )
        if stdout == "0":
            Prompt.msgbox(
                "ARCHI",
                "Looks like we are not running in UEFI mode, please reboot and enable UEFI mode",
            )
            sys.exit(1)

    # Select disk
    if not args.disk:
        _, stdout, _ = shell("lsblk -d -p -n -l -o NAME,SIZE -e 7,11")
        while True:
            rc, disk = Prompt.menu(
                "ARCHI",
                "Select disk device to use for installation",
                stdout.split("\n"),
            )
            if rc == 0:
                args.disk = disk.split()[0]
                break
    logging.debug(f"disk=`{args.disk}`")

    # Partition disk
    rc, _ = Prompt.yesno("ARCHI", f"Would you like to wipe and partition {args.disk}?")
    if rc == 0:
        shell(f"wipefs -a -f {args.disk}", dryrun=True)  # TODO: check rc
        Prompt.msgbox("ARCHI", f"Wiped {args.disk}")
