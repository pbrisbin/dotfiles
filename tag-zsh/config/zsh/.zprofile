function start_ssh_agent() {
  local ssh_env="$XDG_CACHE_HOME/ssh-env"

  if pgrep ssh-agent >/dev/null; then
    source "$ssh_env"
  else
    ssh-agent | grep -Fv echo > "$ssh_env"
    source "$ssh_env"
    ssh-add
  fi
}

export XDG_CACHE_HOME="$HOME"/.cache
export XDG_CONFIG_HOME="$HOME"/.config
export XDG_DATA_HOME="$HOME"/.local/share

export GNUPGHOME="$XDG_CONFIG_HOME"/gnupg
export LESSHISTFILE="$XDG_CACHE_HOME"/lesshist
export NOTITLE=1 # avoid broken grml precmd hook
export NOTMUCH_CONFIG="$XDG_CONFIG_HOME"/notmuchrc
export RCRC="$XDG_CONFIG_HOME"/rcrc
export SCREENRC="$XDG_CONFIG_HOME"/screenrc
export XAUTHORITY="$XDG_RUNTIME_DIR"/X11-authority

# With GPG 2.1+ the GPG_AGENT_INFO variable is not set and any value present is
# ignored. Mutt, however, checks for its existence before attempting to use the
# agent for authentication. It doesn't use the value, so we just need something
# unempty to tell mutt it's OK to use the agent.
#
# Eventual fix: http://dev.mutt.org/trac/attachment/ticket/3715/gpg21.patch
#
export GPG_AGENT_INFO=x

path=(
  ~/.local/bin
  ./.cabal-sandbox/bin
  ~/.cabal/bin
  ~/.gem/ruby/2.2.0/bin
  $path
)

start_ssh_agent

if [[ $TTY == /dev/tty1 ]] && [[ -z $DISPLAY ]]; then
  # Note: Since Xorg 1.16, redirecting stderr is unsupported.
  exec startx # 2>! "$XDG_RUNTIME_DIR"/xsession-errors
fi
