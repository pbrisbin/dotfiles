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

stack-test() {
  stack build --dependencies-only .
  stack build --fast --pedantic --test .
}

stack-watch() {
  stack build --fast --pedantic --test --file-watch .
}

stack-watch-match() {
  # N.B. funny characters in the match won't work well
  stack build --fast --pedantic --test --file-watch --test-arguments "-m $*" .
}

stack-watch-rerun() {
  stack build --fast --pedantic --test --file-watch --test-arguments \
    '--rerun --failure-report=TESTREPORT --rerun-all-on-success' .
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
  ~/code/restyled-io/
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

cd - >/dev/null
