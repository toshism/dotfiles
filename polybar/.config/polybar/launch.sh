#!/bin/bash

# Basic script to kill all old bars and launch new.

# Terminate already running bad instances
killall -q polybar

# Wait until the processes have been shut down
# while grep -x polybar >/dev/null; do sleep 1; done

IFS=$'\n'
for monitor in $(polybar -m)
do
    MONITOR=$(echo $monitor|sed -e 's/:.*$//g') polybar top &
done
