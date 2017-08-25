#!/bin/bash

eval "export $(egrep -z DBUS_SESSION_BUS_ADDRESS /proc/$(pgrep -o -u $LOGNAME i3)/environ)";
#notify-send -t 3000 "Checking mail..."
killall mbsync &>/dev/null
/usr/bin/mbsync uniregistry
#notify-send -t 3000 "$(/usr/local/bin/notmuch new)"
/usr/local/bin/notmuch new
/usr/local/bin/afew -tvn
