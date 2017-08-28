# Custom Completions
_git-db() { _git-checkout }
_git-dbf() { _git-checkout }
_git-delete-tag() { compadd "$@" $(git tag) }

# Styles
zstyle ':completion:*:*:hub:*' user-commands ${${(M)${(k)commands}:#git-*}/git-/}
zstyle ':completion:*:sudo:*' command-path $path
zstyle ':prompt:grml:left:setup' items percent
zstyle ':prompt:grml:right:setup' items rc vcs
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:git:*' actionformats '%c%u %r:%b|%a'
zstyle ':vcs_info:git:*' check-for-changes true
zstyle ':vcs_info:git:*' formats '%c%u %r:%b'

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
export MANWIDTH=80

HISTSIZE=500000
SAVEHIST=$HISTSIZE

cdpath=(
  ~
  ~/code
  ~/code/frontrowed
  ~/code/pbrisbin
  $cdpath
)
path=(
  ~/.local/bin
  ~/.gem/ruby/2.4.0/bin
  $path
)

# Bindings & Options
bindkey '^[[Z' reverse-menu-complete       # Shift-Tab
bindkey -M viins '^?' backward-delete-char # Backspace

setopt inc_append_history
setopt vi
unsetopt nomatch

# Completion
autoload -U +X compinit && compinit
autoload -U +X bashcompinit && bashcompinit
eval "$(stack --bash-completion-script stack)"

# SSH Agent
ssh_env="$HOME/.ssh/agent-env"

if pgrep ssh-agent >/dev/null; then
  source "$ssh_env"
else
  ssh-agent | grep -Fv echo > "$ssh_env"
  source "$ssh_env"
  ssh-add
fi

# Start X
if [[ $TTY == /dev/tty1 ]] && [[ -z $DISPLAY ]]; then
  exec startx
fi

cd - >/dev/null
