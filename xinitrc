msu
xrdb ~/.Xresources

dunst \
  -fn "Dejavu 12" \
  -follow keyboard \
  -format "<b>%s</b>\n\n%b" \
  -geometry "300x5-30+20" \
  -horizontal_padding 16 \
  -markup \
  -padding 18 \
  -separator_height 1 \
  -word_wrap &

urxvtd &
xautolock -time 15 -locker slock -secure &

exec xmonad
