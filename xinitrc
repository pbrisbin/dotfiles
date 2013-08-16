#!/bin/sh
[[ -r ~/.monitors ]] && . ~/.monitors

eval $(cat ~/.fehbg)

xset s off
xset fp+ /usr/share/fonts/local
xsetroot -cursor_name left_ptr
setxkbmap -option terminate:ctrl_alt_bksp

xcompmgr -fF &
android-receiver &

urxvtd -q -f -o

xmonad
