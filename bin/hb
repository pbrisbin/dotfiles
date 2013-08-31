#!/usr/bin/env bash
#
# pbrisbin 2012
#
###
(( $# )) || { echo 'usage: hb [ rip <name> | convert <file> ] [ options ]' >&2; exit 1; }

case "$1" in
  rip)     src='/dev/sr0'
           dst="$2.m4v" ;;
  convert) src="$2"
           dst="$(basename "${src%.*}").m4v" ;;
esac

shift 2
HandBrakeCLI -Z "High Profile" "$@" -i "$src" -o "/mnt/media/Movies/$dst"
