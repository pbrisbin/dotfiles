# vim: ft=sh

# Source any installed helpers, like /etc/X11/xinit/xinitrc does
if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/?*.sh; do
    # shellcheck disable=SC1090
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

systemctl --user start xrdb
systemctl --user start polybar xautolock

exec xmonad
