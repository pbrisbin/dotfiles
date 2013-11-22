#!/bin/sh
[ -r ~/.xinitrc.local ] && . ~/.xinitrc.local

xrdb ~/.config/Xresources

feh --bg-max ~/.local/share/background.png

xset s off
xsetroot -cursor_name left_ptr
urxvtd -q -f -o

xmonad
