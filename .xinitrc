#!/bin/sh

eval `cat $HOME/.fehbg` &                   # set background
numlockx on &                               # numlock on
setxkbmap -option terminate:ctrl_alt_bksp & # add C-A-BS to kill X
urxvtd -q -f -o &                           # start urxvt daemon
xcompmgr -fF &                              # allow transparency
xrdb $HOME/.Xdefaults &                     # load Xdefaults
xset fp+ /usr/share/fonts/local &           # add local fonts
#xset s 1200 1200 &                          # set monitor to sleep 20 mins
xset s off &                                # disable screensaver
xsetroot -cursor_name left_ptr              # required for xmonad's cursor

(sleep 3 && xmodmap $HOME/.Xmodmap) &       # remove capslock - delay required

xmonad                                      # start xmonad
