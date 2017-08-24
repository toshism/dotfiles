#!/bin/bash

notify-send -t 3000 "Checking mail..."
killall mbsync &>/dev/null
/usr/bin/mbsync uniregistry
notify-send -t 3000 "$(/usr/local/bin/notmuch new)"
/usr/bin/afew -tvn
