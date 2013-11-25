# Prompt {{{
#
# Based on pure: https://github.com/sindresorhus/pure
#
_git_info() {
  vcs_info

  printf "$vcs_info_msg_0_" # branch name/action

  command git rev-parse --is-inside-work-tree &>/dev/null || return
  command git diff --quiet --ignore-submodules HEAD &>/dev/null

  (( $? == 1 )) && printf '*'
}

_ruby_info() {
  which ruby &>/dev/null || return

  printf ' ruby-%s' "$(ruby --version | cut -d' ' -f2)"
}

prompt_pbr_setup() {
  prompt_opts=( cr subst percent )

  autoload -Uz vcs_info

  zstyle ':vcs_info:*' enable git
  zstyle ':vcs_info:git*' formats ' %b'
  zstyle ':vcs_info:git*' actionformats ' %b|%a'

  [[ -n "$SSH_CONNECTION" ]] && prompt_pbr_username=' %n@%m'

  PROMPT='
%F{blue}%~%F{8}$(_ruby_info)$(_git_info)$prompt_pbr_username%f
%(?.%F{magenta}.%F{red})❯%f '
}

prompt_pbr_setup "$@"

prompt_themes+=( pbr )
prompt_themes=( "${(@on)prompt_themes}" )

prompt pbr
# }}}

_git-delete-branch() { _git-checkout }

zstyle ':completion:*:sudo:*' command-path $path

HISTSIZE=12000
SAVEHIST=50000

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
export MANWIDTH=80
export SCREENRC="$HOME/.config/screenrc"

setopt inc_append_history
setopt vi

store_current_directory() {
  echo "$PWD" >! ~/.cache/current-directory
}

add-zsh-hook chpwd store_current_directory

if [[ -r ~/.cache/current-directory ]]; then
  cd $(< ~/.cache/current-directory)
fi
