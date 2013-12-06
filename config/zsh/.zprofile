if pgrep "ssh-agent" >/dev/null; then
  source ~/.ssh-env
else
  ssh-agent | grep -Fv 'echo' > ~/.ssh-env
  source ~/.ssh-env
  ssh-add
fi

path=( "$HOME/.local/bin" "$HOME/.cabal/bin" $path )

source "$HOME/.secrets"
export XDG_CONFIG_HOME="$HOME"/.config
export SCREENRC="$XDG_CONFIG_HOME"/screenrc
export GNUPGHOME="$XDG_CONFIG_HOME"/gnupg
export NOTITLE=1 # avoid broken grml precmd hook

if [[ $TTY == /dev/tty1 ]] && [[ -z $DISPLAY ]]; then
  exec startx 2>! ~/.xsession-errors
fi
