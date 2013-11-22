#!/bin/sh
[ -r ~/.xinitrc.local ] && . ~/.xinitrc.local

xrdb ~/.Xresources

feh --bg-max ~/.background.png

xset s off
xsetroot -cursor_name left_ptr
urxvtd -q -f -o

xmonad
