#!/bin/sh

android-receiver --handler dzen-handler &   # display android-notifications
eval `cat $HOME/.fehbg` &                   # set background
setxkbmap -option terminate:ctrl_alt_bksp & # add C-A-BS to kill X
urxvtd -q -f -o &                           # start urxvt daemon
xcompmgr -fF &                              # allow transparency
xrdb $HOME/.Xdefaults &                     # load Xdefaults
xset fp+ /usr/share/fonts/local &           # add local fonts
xset s 7200 7200 &                          # set monitor to sleep 2 hours
xset s off &                                # disable screensaver
xsetroot -cursor_name left_ptr              # required for xmonad's cursor

(sleep 3 && xmodmap $HOME/.Xmodmap) &       # adjust keymappings - delay required

xmonad                                      # start xmonad
