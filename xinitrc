# vim: ft=sh

systemctl --user start msu
systemctl --user start xrdb
systemctl --user start dunst polybar xautolock

# Source any installed helpers, like /etc/X11/xinit/xinitrc does
if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/?*.sh; do
    # shellcheck disable=SC1090
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

exec xmonad
