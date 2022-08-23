#!/bin/sh
# xrandr --output HDMI2 --mode 1920x1080 --pos 1920x0 --rotate left --output HDMI1 --primary --mode 1920x1080 --pos 0x400 --rotate normal
xrandr --output DP1 --primary --mode 1920x1080 --pos 0x0 --rotate normal --output DP2 --mode 1920x1080 --pos 1920x0 --rotate normal --output HDMI1 --off --output VIRTUAL1 --off
