# Custom Completions
_git-db() { _git-checkout }
_git-dbf() { _git-checkout }
_git-delete-tag() { compadd "$@" $(git tag) }

# Styles
zstyle ':completion:*:*:hub:*' user-commands ${${(M)${(k)commands}:#git-*}/git-/}
zstyle ':completion:*:sudo:*' command-path $path
zstyle ':prompt:grml:left:setup' items percent rc
zstyle ':prompt:grml:right:setup' items path vcs
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:git:*' actionformats '[%r:%b|%a]'
zstyle ':vcs_info:git:*' formats '[%r:%b]'

# Aliases & Functions
alias g=git
alias git=hub; compdef hub=git
alias p="sudo pacman"

clone() {
  case "$1" in
    */*)
      target=$HOME/code/$1
      mkdir -p "$(dirname "$target")"
      git clone "git@github.com:$1" "$target"
      cd "$target"
      ;;
  esac
}

# Exports
export XDG_CACHE_HOME=$HOME/.cache
export XDG_CONFIG_HOME=$HOME/.config
export XDG_DATA_HOME=$HOME/.local/share

export BROWSER=chromium
export GNUPGHOME="$XDG_CONFIG_HOME"/gnupg
export GPG_TTY="$(tty)"
export FZF_DEFAULT_COMMAND='
  ( git ls-tree -r --name-only HEAD ||
    find . -path "*/\.*" -prune -o -type f -print -o -type l -print |
    sed s/^..//
  ) 2>/dev/null
'
export MANWIDTH=80

HISTSIZE=500000
SAVEHIST=$HISTSIZE

cdpath=(
  ~
  ~/code
  ~/code/freckle
  ~/code/pbrisbin
  ~/code/restyled-io/
  $cdpath
)
path=(
  ~/.local/bin
  $(ruby -r rubygems -e "puts Gem.user_dir")/bin
  $path
)

# Bindings & Options
bindkey '^[[Z' reverse-menu-complete       # Shift-Tab
bindkey -M viins '^?' backward-delete-char # Backspace

# Use *all* of command-line so far when navigating history up/down
bindkey '^[[A' history-beginning-search-backward
bindkey '^[[B' history-beginning-search-forward
bindkey -M vicmd 'k' history-beginning-search-backward
bindkey -M vicmd 'j' history-beginning-search-forward

# v to edit command in $VISUAL or $EDITOR
autoload -U edit-command-line
zle -N edit-command-line
bindkey -M vicmd v edit-command-line

setopt inc_append_history
setopt vi
unsetopt nomatch

# Completion
autoload -U +X compinit && compinit
autoload -U +X bashcompinit && bashcompinit
eval "$(stack --bash-completion-script stack)"
source /usr/bin/aws_zsh_completer.sh

# SSH Agent
ssh_env="$HOME/.ssh/agent-env"

if pgrep ssh-agent >/dev/null; then
  source "$ssh_env"
else
  ssh-agent | grep -Fv echo > "$ssh_env"
  source "$ssh_env"

  # Use pass(1), via wrapper script, to unlock SSH key
  DISPLAY=99 SSH_ASKPASS="$HOME/.local/bin/ssh-askpass" ssh-add </dev/null
fi

# Start X
if [[ $TTY == /dev/tty1 ]] && [[ -z $DISPLAY ]]; then
  exec startx
fi

popd >/dev/null
