#!/usr/bin/env python

"""
Charge now utility
"""

from dotfiles import shell
import argparse

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
        add_help=False,
    )

    positional_parser = parser.add_argument_group("positional arguments")
    positional_parser.add_argument(
        "BATTERY",
        help="battery",
    )

    required_parser = parser.add_argument_group("required arguments")

    optional_parser = parser.add_argument_group("optional arguments")
    optional_parser.add_argument(
        "-h",
        "--help",
        help="show this help message and exit",
        action="help",
        default=argparse.SUPPRESS,
    )

    args = parser.parse_args()

    with open(f"/sys/class/power_supply/{args.BATTERY}/energy_now") as energy_now, open(
        f"/sys/class/power_supply/{args.BATTERY}/energy_full"
    ) as energy_full:
        data = (
            int(energy_now.readline().split()[0]),
            int(energy_full.readline().split()[0]),
        )
        print(round(data[0] / data[1] * 100, 2))
