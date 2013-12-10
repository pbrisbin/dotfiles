#!/bin/sh
[ -r ~/.xinitrc.local ] && . ~/.xinitrc.local

xrdb "$XDG_CONFIG_HOME"/Xresources

feh --no-fehbg --bg-max "$XDG_DATA_HOME"/background.png

xset s off
xsetroot -cursor_name left_ptr
urxvtd -q -f -o

xmonad
