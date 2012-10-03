# setup
eval "$(dircolors -b $HOME/.dir_colors)"

# aliases
alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias v='vim'
alias b='bundle'
alias be='b exec'
alias g=git
alias gspull='g stash && g svn rebase && g stash apply'
alias gspush='g stash && g svn dcommit && g stash apply'

# keymap fix
bindkey -M "viins" "^[[3" delete-char
bindkey -M "viins" "^?"   backward-delete-char

# completion
autoload -Uz compinit && compinit -i

zstyle ':completion::complete:*' use-cache on
zstyle ':completion::complete:*' cache-path "${ZDOTDIR:-$HOME}/.zcompcache"

zstyle ':completion:*:*:*:*:*' menu select
zstyle ':completion:*:matches' group 'yes'
zstyle ':completion:*:options' description 'yes'

zstyle -e ':completion:*:hosts' hosts 'reply=(
  ${=${${(f)"$(cat {/etc/ssh_,~/.ssh/known_}hosts(|2)(N) 2>/dev/null)"}%%[#| ]*}//,/ }
  ${=${(f)"$(cat /etc/hosts(|)(N) <<(ypcat hosts 2>/dev/null))"}%%\#*}
  ${=${${${${(@M)${(f)"$(cat ~/.ssh/config 2>/dev/null)"}:#Host *}#Host }:#*\**}:#*\?*}}
)'

zstyle ':completion:*:*:*:*:processes' command 'ps -u $USER -o pid,user,comm -w'
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:*:kill:*' force-list always
zstyle ':completion:*:*:kill:*' insert-ids single

zstyle ':completion:*:manuals' separate-sections true
zstyle ':completion:*:manuals.(^1*)' insert-sections true

# prompt
autoload -Uz promptinit && promptinit
prompt minimal
