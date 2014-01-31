#!/bin/sh

msu # Monitor Setup Utility

xrdb "$XDG_CONFIG_HOME"/Xresources
xset s off
xsetroot -cursor_name left_ptr
xautolock -time 5 -locker slock -detectsleep
urxvtd -q -f -o
android-receiver &

xmonad
