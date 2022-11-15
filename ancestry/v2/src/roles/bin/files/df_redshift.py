#!/usr/bin/env python

"""
Redshift utility
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
        "CONFIGURATION",
        help="configuration",
        choices=["desktop", "laptop", "remote"],
    )
    positional_parser.add_argument(
        "ACTION",
        help="action",
        choices=["start", "restart", "stop"],
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
    optional_parser.add_argument(
        "-n", "--dryrun", help="run in dryrun mode", action="store_true"
    )

    args = parser.parse_args()

    if args.CONFIGURATION == "desktop":
        if args.ACTION == "start" or args.ACTION == "restart":
            shell('df_launcher.py "redshift -P -t 3500:2500 -l 40.7128:-74.0060" -d')
        elif args.ACTION == "stop":
            shell("killall -I -q -w redshift")
            shell("redshift -x")
    elif args.CONFIGURATION == "laptop":
        if args.ACTION == "start" or args.ACTION == "restart":
            shell('df_launcher.py "redshift -P -t 5500:2500 -l 40.7128:-74.0060" -d')
        elif args.ACTION == "stop":
            shell("killall -I -q -w redshift")
            shell("redshift -x")
