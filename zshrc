# Styles
zstyle ':completion:*:*:hub:*' user-commands ${${(M)${(k)commands}:#git-*}/git-/}
zstyle ':completion:*:sudo:*' command-path $path

# Aliases & Functions
alias g=git
alias git=hub; compdef hub=git

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
HISTFILE=~/.zsh_history

cdpath=( $HOME $HOME/code $cdpath )
path=( ~/.local/bin $path )

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
unset ssh_env

# Start X
if [[ $TTY == /dev/tty1 ]] && [[ -z $DISPLAY ]]; then
  exec startx
fi