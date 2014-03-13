#!/usr/bin/env bash
#
# pbrisbin 2013 - wrap handbrake to handle commonly needed scenarios.
#
###
if (( $# < 2 )); then
  printf "usage: hb [rip <name>|convert <file>] [options]\n" >&2
  exit 1
fi

case "$1" in
  rip)     src='/dev/sr0'
           dst="$2.m4v" ;;
  convert) src="$2"
           dst="$(basename "${src%.*}").m4v" ;;
esac

shift 2
HandBrakeCLI -Z "High Profile" "$@" -i "$src" -o "/mnt/media/Movies/$dst"
