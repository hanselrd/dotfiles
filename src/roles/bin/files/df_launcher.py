#!/usr/bin/env python

"""
Launcher utility
"""

from dotfiles import shell, slugify
import argparse
import atexit
import os
import sys

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
        add_help=False,
    )

    positional_parser = parser.add_argument_group("positional arguments")
    positional_parser.add_argument("COMMAND", help="command to launch")

    required_parser = parser.add_argument_group("required arguments")

    optional_parser = parser.add_argument_group("optional arguments")
    optional_parser.add_argument(
        "-h",
        "--help",
        help="show this help message and exit",
        action="help",
        default=argparse.SUPPRESS,
    )
    optional_parser_group = optional_parser.add_mutually_exclusive_group()
    optional_parser_group.add_argument(
        "-d",
        "--daemonize",
        help="run command as a daemon",
        action="store_true",
    )
    optional_parser_group.add_argument(
        "-r",
        "--restart",
        help="restart command automatically",
        action="store_true",
    )
    optional_parser.add_argument(
        "-n", "--dryrun", help="run in dryrun mode", action="store_true"
    )

    args = parser.parse_args()

    EXECUTABLE = args.COMMAND.split()[0]

    shell(f"killall -I -q -w {EXECUTABLE}", dryrun=args.dryrun)

    if args.daemonize:
        pidfile = f"/tmp/{slugify(args.COMMAND)}.pid"

        try:
            with open(pidfile, "r") as pf:
                pid = int(pf.read().strip())
        except IOError:
            pid = None

        if pid:
            sys.stderr.write(
                f"pidfile {pidfile} already exists. Daemon already running?\n"
            )
            sys.exit(1)

        try:
            pid = os.fork()
            if pid > 0:
                sys.exit(0)
        except OSError as e:
            sys.stderr.write(f"fork #1 failed: {e})\n")
            sys.exit(1)

        os.chdir("/")
        os.setsid()
        os.umask(0)

        try:
            pid = os.fork()
            if pid > 0:
                sys.exit(0)
        except OSError as e:
            sys.stderr.write(f"fork #2 failed: {e})\n")
            sys.exit(1)

        sys.stdout.flush()
        sys.stderr.flush()

        si = open(os.devnull, "r")
        so = open(os.devnull, "a+")
        se = open(os.devnull, "a+")

        os.dup2(si.fileno(), sys.stdin.fileno())
        os.dup2(so.fileno(), sys.stdout.fileno())
        os.dup2(se.fileno(), sys.stderr.fileno())

        atexit.register(lambda: os.remove(pidfile))

        pid = f"{os.getpid()}"
        with open(pidfile, "w+") as f:
            f.write(f"{pid}\n")

        shell(f"{args.COMMAND}", progress=False, dryrun=args.dryrun)
    elif args.restart:
        while True:
            shell(f"{args.COMMAND}", progress=False, dryrun=args.dryrun)
    else:
        shell(f"{args.COMMAND}", dryrun=args.dryrun)
