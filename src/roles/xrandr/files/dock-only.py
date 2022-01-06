#!/usr/bin/env python

from dotfiles import shell
import sys

if __name__ == "__main__":
    rc, PRIMARY, _ = shell("xrandr --query | grep '\\bprimary\\b' | cut -d\" \" -f1")

    rc, stdout, _ = shell(
        "xrandr --query | grep '\\bconnected\\b' | grep -v '\\bprimary\\b' | cut -d\" \" -f1"
    )
    CONNECTED_MONITORS = stdout.split("\n")

    if rc == 0:
        DP__1_5 = [c for c in CONNECTED_MONITORS if "DP" in c and "-1-5" in c][0]
        DP_ = [c for c in CONNECTED_MONITORS if "DP" in c and "-1-5" not in c][0]
    else:
        sys.exit(1)

    rc, stdout, _ = shell(
        "xrandr --query | grep '\\bdisconnected\\b' | grep -v '\\bprimary\\b' | cut -d\" \" -f1"
    )
    DISCONNECTED_MONITORS = stdout.split("\n")

    shell(
        f"xrandr --output {PRIMARY} --off --output {DP__1_5} --mode 1920x1080 --pos 0x0 --rotate normal --output {DP_} --mode 1920x1080 --pos 1920x0 --rotate normal {' '.join([f'--output {c} --off' for c in DISCONNECTED_MONITORS])}"
    )
