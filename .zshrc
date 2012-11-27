# environment
eval "$(dircolors -b $HOME/.dir_colors)"
export GREP_COLOR='1;32'
export GREP_OPTIONS='--color=auto'
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;38;5;74m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[38;5;246m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[04;38;5;146m'

# aliases
alias ls='ls --color=auto'

alias v='vim'
alias g='git'
alias p='sudo pacman'

alias b='bundle'
alias be='b exec'

alias rip='dvd2iso -o /mnt/media/Rips/%s.iso'
alias piso='mplayer dvd://1 -dvd-device'
alias pdvd='mplayer dvdnav:// -dvd-device /dev/sr0 -mouse-movements'

alias hdocs="$BROWSER $HOME/.cabal/share/doc/index.html"

# keymap fixes
bindkey '^[[Z' reverse-menu-complete       # Shift-Tab
bindkey '^[[3~' delete-char                # Delete
bindkey -M viins '^?' backward-delete-char # Backspace

# completion
autoload -Uz compinit && compinit -i

zstyle ':completion:*:*:*:*:*' menu select
zstyle ':completion::complete:*' use-cache on
zstyle ':completion::complete:*' cache-path "${ZDOTDIR:-$HOME}/.zcompcache"
zstyle ':completion:*:*:*:*:processes' command 'ps -u $USER -o pid,user,comm -w'

zstyle -e ':completion:*:hosts' hosts 'reply=(
  ${=${${(f)"$(cat {/etc/ssh_,~/.ssh/known_}hosts(|2)(N) 2>/dev/null)"}%%[#| ]*}//,/ }
  ${=${${${${(@M)${(f)"$(cat ~/.ssh/config 2>/dev/null)"}:#Host *}#Host }:#*\**}:#*\?*}}
)'

# setup command not found
if (( $+commands[pkgfile] )); then
  source /usr/share/doc/pkgfile/command-not-found.zsh
fi

# terminal title
autoload -Uz add-zsh-hook

function set-titles-precmd() {
  title_string="${PWD/$HOME/~}"

  case $TERM in
    screen*)
      printf "\ek%s\e\\" "$title_string"
      ;;
    ((x|a|ml|dt|E)term*|(u|)rxvt*)
      printf "\e]1;%s\a" "$title_string" # tab
      printf "\e]2;%s\a" "$title_string" # title
      ;;
  esac
}
add-zsh-hook precmd set-titles-precmd

# prompt
autoload -Uz promptinit && promptinit
prompt minimal
