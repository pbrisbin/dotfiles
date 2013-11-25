source "$HOME/.secrets"

if [[ -e /usr/share/chruby ]]; then
  source /usr/share/chruby/chruby.sh
  source /usr/share/chruby/auto.sh

  chruby $(cat ~/.ruby-version)
fi

if pgrep "ssh-agent" >/dev/null; then
  source ~/.ssh-env
else
  ssh-agent | grep -Fv 'echo' > ~/.ssh-env
  source ~/.ssh-env
  ssh-add
fi

cdpath=( "$HOME" "$HOME/Code" $cdpath )

fpath=( "$HOME/.zfunctions" $fpath )

path=( "$HOME/.local/bin" "$HOME/.cabal/bin" $path )

if [[ $TTY == /dev/tty1 ]] && [[ -z $DISPLAY ]]; then
  exec startx 2>! ~/.xsession-errors
fi

export NOTITLE=1 # avoid broken grml precmd hook
