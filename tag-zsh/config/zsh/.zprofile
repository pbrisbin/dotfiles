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

path=(
  ~/.local/bin
  ./.cabal-sandbox/bin
  ~/.cabal/bin
  ~/.gem/ruby/2.1.5/bin
  $path
)

start_ssh_agent

if [[ $TTY == /dev/tty1 ]] && [[ -z $DISPLAY ]]; then
  # Note: Since Xorg 1.16, redirecting stderr is unsupported.
  exec startx # 2>! "$XDG_RUNTIME_DIR"/xsession-errors
fi
