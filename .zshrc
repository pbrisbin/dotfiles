export OMZ="$HOME/.oh-my-zsh"

zstyle ':omz:*:*'              case-sensitive 'no'
zstyle ':omz:*:*'              color          'yes'
zstyle ':omz:module:editor'    dot-expansion  'no'
zstyle ':omz:module:editor'    keymap         'vi'
zstyle ':omz:module:prompt'    theme          'pbrisbin'
zstyle ':omz:module:terminal'  auto-title     'yes'
zstyle ':omz:module:ssh-agent' identities     'id_rsa'          \
                                              'id_rsa.pbrisbin' \
                                              'id_rsa.github'   \
                                              'id_rsa.ideeli'

zstyle ':omz:load' omodule 'environment' \
                           'terminal'    \
                           'editor'      \
                           'completion'  \
                           'history'     \
                           'directory'   \
                           'spectrum'    \
                           'alias'       \
                           'utility'     \
                           'prompt'      \
                           'archive'     \
                           'git'         \
                           'rails'       \
                           'ruby'        \
                           'screen'      \
                           'ssh-agent'

source "$OMZ/init.zsh"

# start X when appropriate
if [[ $(tty) == /dev/tty1 ]] && (( $UID )) && [[ -z $DISPLAY ]]; then
  exec startx
fi
