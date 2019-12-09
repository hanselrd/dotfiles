#!/usr/bin/env bash

# Terminate already running instances
killall -q $1

# Wait until the processes have been shut down
while pgrep -u $UID -x $1 >/dev/null; do sleep 1; done

# Launch
$@ &

echo "$1 launched..."
