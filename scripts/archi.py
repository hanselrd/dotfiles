#!/usr/bin/env python

# import logging
import argparse
import enum
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
    cmd: str, input: str = None, stdout: bool = True, stderr: bool = True
) -> typing.Tuple[int, str, str]:
    print(f"cmd={cmd}")
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
    def msgbox(title: str, text: str):
        return shell(f'whiptail --title "{title}" --msgbox "{text}" 0 0', stdout=False)

    @staticmethod
    def yesno(title: str, text: str):
        return shell(f'whiptail --title "{title}" --yesno "{text}" 0 0', stdout=False)

    @staticmethod
    def inputbox(title: str, text: str, default: str = None):
        return shell(
            f'whiptail --title "{title}" --inputbox "{text}" 0 0 "{default if default else str()}"',
            stdout=False,
        )

    @staticmethod
    def passwordbox(title: str, text: str):
        return shell(
            f'whiptail --title "{title}" --passwordbox "{text}" 8 0', stdout=False
        )

    @staticmethod
    def menu(title: str, text: str, choices: typing.List[str], default: str = None):
        choices = [f'"{c}" ""' for c in choices]
        return shell(
            f'whiptail --title "{title}" --default-item "{default if default else str()}" --menu "{text}" 0 0 0 {" ".join(choices)}',
            stdout=False,
        )


if __name__ == "__main__":
    # parser = argparse.ArgumentParser(
    #     add_help=False, formatter_class=argparse.ArgumentDefaultsHelpFormatter
    # )

    # positional_parser = parser.add_argument_group("positional arguments")
    # positional_parser.add_argument("disk", help="disk to use for installation")

    # required_parser = parser.add_argument_group("required arguments")
    # required_mutex_parser = required_parser.add_mutually_exclusive_group(required=True)
    # required_mutex_parser.add_argument(
    #     "-u", "--uefi", help="use UEFI mode", action="store_true"
    # )
    # required_mutex_parser.add_argument(
    #     "-b", "--bios", help="use BIOS mode", action="store_true"
    # )
    # required_parser.add_argument(
    #     "-e", "--username", help="username of the user account", required=True
    # )
    # # required_parser.add_argument('-t','--test',help='testing test',nargs='*',default=[],required=True)

    # optional_parser = parser.add_argument_group("optional arguments")
    # optional_parser.add_argument(
    #     "-h",
    #     "--help",
    #     help="show this help message and exit",
    #     action="help",
    #     default=argparse.SUPPRESS,
    # )
    # optional_parser.add_argument(
    #     "-o", "--boot", help="size of boot/efi partition", default="1G"
    # )
    # optional_parser.add_argument(
    #     "-s", "--swapfile", help="size of swapfile", default="4G"
    # )
    # optional_parser.add_argument(
    #     "-l",
    #     "--loglevel",
    #     help="set logging level",
    #     type=LogLevel,
    #     choices=list(LogLevel),
    #     default=LogLevel.Info,
    # )

    # args = parser.parse_args()
    # print(f'args={args}')

    # Check for UEFI
    _, stdout, _ = shell("find /sys/firmware/efi/efivars -print 2>/dev/null | wc -l")
    if stdout == "0":
        Prompt.msgbox(
            "ARCHI",
            "Looks like we are not running in UEFI mode, please reboot and enable UEFI mode",
        )
        sys.exit(1)

    # Select disk
    _, stdout, _ = shell("lsblk -d -p -n -l -o NAME,SIZE -e 7,11")
    while True:
        rc, _, device = Prompt.menu(
            "ARCHI", "Select disk device to use for installation", stdout.split("\n")
        )
        if rc == 0:
            device = device.split()[0]
            break
    print(f"device={device}")

    # Partition disk
    rc, _, _ = Prompt.yesno("ARCHI", f"Would you like to wipe and partition {device}?")
    if rc == 0:
        print(f"$ wipefs -a -f {device}")
        Prompt.msgbox("ARCHI", f"Wiped {device}")
