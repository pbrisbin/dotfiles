zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' stagedstr '%F{yellow}+%f'
zstyle ':vcs_info:*' unstagedstr '%F{green}*%f'
zstyle ':vcs_info:*' formats \
  '%%B%F{cyan}[%F{red}%b%c%u%F{cyan}]%f%%b ' '%r:%b'
zstyle ':vcs_info:*' actionformats \
  '%%B%F{cyan}[%F{red}%b%c%u%F{cyan}|%F{yellow}%a%F{cyan}]%f%%b ' '%r:%b'

zstyle ':completion:*:sudo:*' command-path $path

HISTSIZE=12000
SAVEHIST=50000

alias v='vim'
alias g='git'
alias p='sudo pacman'
alias b='bundle'
alias be='b exec'
alias irc='screen -R -D -S irc'
alias rtor='screen -R -D -S rtor'
alias piso='mplayer dvd://1 -dvd-device'
alias pdvd='mplayer dvdnav:// -dvd-device /dev/sr0 -mouse-movements'
alias git=hub; compdef hub=git

bindkey '^[[Z' reverse-menu-complete       # Shift-Tab
bindkey -M viins '^?' backward-delete-char # Backspace

export BROWSER='chromium'
export MANWIDTH=80

setopt inc_append_history
setopt vi
