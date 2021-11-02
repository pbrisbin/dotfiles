# vim: ft=sh
msu
xrdb ~/.Xresources
dunst &
polybar base &
xautolock -time 15 -locker slock -secure &
exec xmonad
