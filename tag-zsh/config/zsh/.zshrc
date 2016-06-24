_git-db() { _git-checkout }
_git-dbf() { _git-checkout }
_git-delete-tag() { compadd "$@" $(git tag) }

zstyle ':completion:*:*:hub:*' user-commands ${${(M)${(k)commands}:#git-*}/git-/}
zstyle ':completion:*:sudo:*' command-path $path

HISTSIZE=50000
SAVEHIST=$HISTSIZE
HISTFILE="$ZDOTDIR"/.zsh_history

alias v='vim'
alias g='git'
alias p='sudo pacman'
alias b='bundle'
alias be='b exec'
alias piso='mplayer dvd://1 -dvd-device'
alias pdvd='mplayer dvdnav:// -dvd-device /dev/sr0 -mouse-movements'
alias git=hub; compdef hub=git

bindkey '^[[Z' reverse-menu-complete       # Shift-Tab
bindkey -M viins '^?' backward-delete-char # Backspace

export BROWSER='chromium'
export GPG_TTY="$(tty)"
export MANWIDTH=80

setopt inc_append_history
setopt vi

autoload -U +X compinit && compinit
autoload -U +X bashcompinit && bashcompinit
eval "$(stack --bash-completion-script stack)"

((NOCD)) || cd - >/dev/null

prompt pbr
