# Custom Completions
_git-db() { _git-checkout }
_git-dbf() { _git-checkout }
_git-delete-tag() { compadd "$@" $(git tag) }
_git-pr() { _gh-pull-request }

# Styles
zstyle ':completion:*:sudo:*' command-path $path
zstyle ':prompt:grml:left:setup' items percent rc
zstyle ':prompt:grml:right:setup' items path vcs
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:git:*' actionformats '[%r:%b|%a]'
zstyle ':vcs_info:git:*' formats '[%r:%b]'

# Aliases & Functions
alias g=git
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

# Autosuggestions
ZSH_AUTOSUGGEST_STRATEGY=(history completion)
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#808080"

for _plugins in /usr/local/share /usr/share/zsh/plugins; do
  _plugin=$_plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

  if [[ -f "$_plugin" ]]; then
    source "$_plugin"
    bindkey '^ ' autosuggest-execute
  fi
done

unset _plugin
unset _plugins

# Completion
autoload bashcompinit && bashcompinit
autoload -Uz compinit && compinit

if [[ -f /usr/bin/aws_completer ]]; then
  complete -C /usr/bin/aws_completer aws
fi

# GPG
export GPG_TTY="$(tty)"

# History
HISTSIZE=500000
SAVEHIST=$HISTSIZE

# Paths
cdpath=(
  ~
  ~/code
  ~/code/freckle
  ~/code/pbrisbin
  ~/code/restyled-io/
  $cdpath
)
fpath=(
  ~/.local/share/zsh/site-functions
  $fpath
)
path=(
  ~/.local/bin
  $(ruby -r rubygems -e "puts Gem.user_dir")/bin
  $path
)

# SSH Agent
ssh_env="$HOME/.ssh/agent-env"

if pgrep ssh-agent >/dev/null; then
  if [[ -f $ssh_env ]]; then
    source "$ssh_env"
  else
    echo "ssh-agent running but $ssh_env not found" >&2
  fi
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
