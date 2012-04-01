# Main settings
zstyle ':omz:editor'            keymap         'vi'
zstyle ':omz:*:*'               color          'yes'
zstyle ':omz:*:*'               case-sensitive 'no'
zstyle ':omz:terminal'          auto-title     'yes'
zstyle ':omz:editor'            dot-expansion  'no'
zstyle ':omz:plugins:ssh-agent' identities     'id_rsa'          \
                                               'id_rsa.pbrisbin' \
                                               'id_rsa.github'   \
                                               'id_rsa.ideeli'

# Plugins
zstyle ':omz:load' plugin 'archive'   \
                          'git'       \
                          'rails'     \
                          'ruby'      \
                          'screen'    \
                          'ssh-agent' \
                          'syntax-highlighting'

# Theme
zstyle ':omz:prompt' theme 'pbrisbin'

# Start OMZ
source "$HOME/.oh-my-zsh/init.zsh"

# Fix backspace
bindkey "^W" backward-kill-word
bindkey "^H" backward-delete-char
bindkey "^U" backward-kill-line
bindkey "^?" backward-delete-char

# Start X when appropriate
if [[ $(tty) == /dev/tty1 ]] && (( $UID )) && [[ -z $DISPLAY ]]; then
  exec startx
fi
