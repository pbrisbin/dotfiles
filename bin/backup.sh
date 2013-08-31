#!/usr/bin/env sh

if [ $UID -ne 0 ]; then
  echo 'You must be root' >&2
  exit 1
fi

if ! which tarsnap >/dev/null; then
  echo 'You must install tarsnap' >&2
  exit 1
fi

timestamp=`date +%Y%m%d.%H:%M:%S`

tarsnap "$@" \
  --keyfile /root/tarsnap.key \
  --cachedir /usr/local/tarsnap-cache \
  -c -f backup-$timestamp /home/patrick /root /etc
