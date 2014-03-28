_git-delete-branch() { _git-checkout }

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
alias git=gh; compdef gh=git

bindkey '^[[Z' reverse-menu-complete       # Shift-Tab
bindkey -M viins '^?' backward-delete-char # Backspace

export BROWSER='chromium'
export MANWIDTH=80

setopt inc_append_history
setopt vi

cd - >/dev/null

prompt pbr
