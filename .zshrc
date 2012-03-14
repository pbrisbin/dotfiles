# Main settings
zstyle ':omz:editor'              keymap         'vi'
zstyle ':omz:*:*'                 color          'yes'
zstyle ':omz:terminal'            auto-title     'yes'
zstyle ':omz:*:*'                 case-sensitive 'no'
zstyle ':omz:editor'              dot-expansion  'no'
zstyle ':omz:alias:ls'            color          'yes'
zstyle ':omz:environment:grep'    color          'yes'
zstyle ':omz:environment:termcap' color          'yes'
zstyle ':omz:load'                plugin         'git'       \
                                                 'rails'     \
                                                 'ruby'      \
                                                 'ssh-agent'
zstyle ':omz:plugins:ssh-agent'   identities     'id_rsa'          \
                                                 'id_rsa.pbrisbin' \
                                                 'id_rsa.github'   \
                                                 'id_rsa.ideeli'
zstyle ':omz:prompt'              theme          'sorin'

# Start OMZ
source "$HOME/.oh-my-zsh/init.zsh"

# Start X when appropriate
if [[ $(tty) == /dev/tty1 ]] && (( $UID )) && [[ -z $DISPLAY ]]; then
  exec startx
fi
