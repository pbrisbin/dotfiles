function ruby_version_prompt() {
  read < <(ruby --version | cut -d ' ' -f 1-2)
}
grml_theme_add_token ruby-version -f ruby_version_prompt '%B%F{black}' '%b%f'
zstyle ':prompt:grml:right:setup' items ruby-version

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

setopt vi
