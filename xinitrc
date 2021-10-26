# vim: ft=sh
msu
xrdb ~/.Xresources
dunst &
xautolock -time 15 -locker slock -secure &
exec xmonad
