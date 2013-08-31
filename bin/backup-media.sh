#!/usr/bin/env sh

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
