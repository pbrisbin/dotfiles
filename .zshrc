#
# Executes commands at the start of an interactive session.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.prezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.prezto/init.zsh"
fi

# Start X appropriately.
if [[ $TTY == /dev/tty1 ]] && (( $UID )) && [[ -z $DISPLAY ]]; then
  exec startx
fi
