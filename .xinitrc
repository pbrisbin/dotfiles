#!/bin/sh
xrandr --output VGA1 --mode 1920x1080
xrandr --output HDMI1 --right-of VGA1

eval `cat $HOME/.fehbg`

xset s off
xset fp+ /usr/share/fonts/local
xsetroot -cursor_name left_ptr
setxkbmap -option terminate:ctrl_alt_bksp

xcompmgr -fF &
android-receiver &

xrdb $HOME/.Xdefaults
xmodmap $HOME/.Xmodmap

urxvtd -q -f -o
synergys --debug ERROR

xmonad
