#!/bin/bash

# Basic script to kill all old bars and launch new.

# Terminate already running bad instances
killall -q polybar

# Wait until the processes have been shut down
# while grep -x polybar >/dev/null; do sleep 1; done

# IFS=$'\n'
# for monitor in $(polybar -m)
# do
#     MONITOR=$(echo $monitor|sed -e 's/:.*$//g') polybar top &
# done

polybar -r top &

# EXTERNAL=$(echo $(polybar -m) | grep -ic "DP-1-8")
# echo $EXTERNAL
# 
# if [ $EXTERNAL -eq 1 ]
# then
#     MONITOR="DP-1-8" polybar top &
# else
#     MONITOR="eDP-1" polybar top &
# fi
# 
# echo $MONITOR
