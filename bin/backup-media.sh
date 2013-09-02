#!/usr/bin/env sh
#
# pbrisbin 2013 - mount a directory, rsync a backup, and unmount.
#
###
set -e

mount --verbose /mnt/backup-media

rsync \
  --archive \
  --delete \
  --delete-excluded \
  --exclude Downloads \
  --verbose \
  /mnt/media/* \
  /mnt/backup-media/

umount --verbose /mnt/backup-media
