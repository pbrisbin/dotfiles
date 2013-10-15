#!/bin/sh
[ -r ~/.xinitrc.local ] && . ~/.xinitrc.local

feh --bg-max ~/.background.png

xset s off
xset fp+ /usr/share/fonts/local
xsetroot -cursor_name left_ptr
setxkbmap -option terminate:ctrl_alt_bksp

android-receiver &

urxvtd -q -f -o

xmonad
