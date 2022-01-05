#!/usr/bin/env python

from dotfiles import shell
import logging
import sys

if __name__ == "__main__":
    logging.basicConfig(
        format="[%(asctime)s] %(levelname)-8s | %(message)s",
        datefmt="%Y-%m-%d %H:%M:%S %z",
        level=logging.DEBUG,
        handlers=[logging.StreamHandler(sys.stdout)],
    )

    rc, PRIMARY, _ = shell("xrandr --query | grep '\\bprimary\\b' | cut -d\" \" -f1")

    rc, stdout, _ = shell(
        "xrandr --query | grep '\\bconnected\\b' | grep -v '\\bprimary\\b' | cut -d\" \" -f1"
    )
    CONNECTED_MONITORS = stdout.split("\n")

    rc, stdout, _ = shell(
        "xrandr --query | grep '\\bdisconnected\\b' | grep -v '\\bprimary\\b' | cut -d\" \" -f1"
    )
    DISCONNECTED_MONITORS = stdout.split("\n")

    shell(
        f"xrandr --output {PRIMARY} --primary --mode 1920x1200 --pos 0x0 --rotate normal {' '.join([f'--output {c} --off' for c in CONNECTED_MONITORS])} {' '.join([f'--output {c} --off' for c in DISCONNECTED_MONITORS])}"
    )
